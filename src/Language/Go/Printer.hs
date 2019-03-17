{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | 
-- Mocking @gofmt@
--
module Language.Go.Printer 
    ( render
    , prettyFile
    )
where

import Prelude

import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, (<+>))
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Language.PureScript.PSString (PSString, decodeString)
import Lens.Micro.Platform (over)

import Cheapskate.Plate (rewriteOf)
import GHC.Exts (the)
import Language.Go.AST
    ( BinOp(..)
    , Block(..)
    , Decl(..)
    , Expr(..)
    , Import(..)
    , Literal(..)
    , UnaryOp(..)
    , extractAnn
    )
import Language.Go.Crash (internalError)
import Language.Go.File (File(..))
import Language.Go.Types (Type(..), subTypes)

import qualified Data.Text.Prettyprint.Doc as Doc
import qualified Language.PureScript.Label as PS (runLabel)
import qualified Language.PureScript.Names as PS

import qualified Language.Go.Constants as C
import qualified Language.Go.Names as Go


render :: Doc a -> Text
render = renderStrict . Doc.layoutPretty Doc.defaultLayoutOptions


prettyFile :: File -> Doc a
prettyFile File{..} = 
    Doc.concatWith (\x y -> x <> blockSep <> y) . mconcat $
        [ -- Package header
          [ "package" <+> prettyPackageName filePackageName ]

          -- Imports
        , [ prettyImports fileImports | not (null fileImports) ]

          -- Declarations
        , fmap (prettyDecl . disqualifyNamedTypes) fileDecls
        ] 
  where
    -- Separate top-level stuff with a single blank line, like @gofmt@
    blockSep :: Doc a
    blockSep = Doc.hardline <> Doc.hardline

    -- Need to remove qualifications for named types belonging to the
    -- current 'File'. The other way we could do this is by plumbing
    -- the 'filePackageName' all the way down to 'prettyQualified' and
    -- performing the check there, but I prefer this solution for now.
    disqualifyNamedTypes :: Decl Type -> Decl Type
    disqualifyNamedTypes = over declTypes $ rewriteOf subTypes $ \case
        NamedType (Go.Qualified (Just pn) tn) 
            | pn == filePackageName -> Just (NamedType (Go.Qualified Nothing tn))
        _ -> Nothing


prettyPackageName :: Go.PackageName -> Doc a
prettyPackageName = Doc.pretty . Go.runPackageName 


-- TODO: Make this prettier
prettyImports :: [Import] -> Doc a
prettyImports imports = 
    "import" <+> Doc.lparen <> Doc.hardline <> 
        indent (hardStack (importLine <$> imports)) <> Doc.hardline <>
    Doc.rparen
  where
    importLine :: Import -> Doc a
    importLine Import{..} = 
        maybe "" ((<> Doc.space) . prettyPackageName) importName <> 
        Doc.dquotes (Doc.pretty importPath)


prettyDecl :: Decl Type -> Doc a
prettyDecl = \case
    VarDecl t ident expr -> 
        "var" <+> prettyGoIdent ident <+> prettyType t <=> prettyExpr expr

    ConstDecl t ident expr -> 
        "const" <+> prettyGoIdent ident <+> prettyType t <=> prettyExpr expr

    FuncDecl t name arg block -> 
        prettyFunc (Just name) t arg block

    TypeDecl tn t -> 
        "type" <+> prettyTypeName tn <+> prettyType t


prettyExpr :: Expr Type -> Doc a
prettyExpr = \case
    VarExpr _ var -> 
        prettyQualified prettyGoIdent var

    AppExpr _ f x -> 
        prettyExpr f <> Doc.parens (prettyExpr x)

    FuncExpr t arg block -> 
        prettyFunc Nothing t arg block

    LiteralExpr _ literal -> 
        prettyLiteral literal

    BlockExpr t block -> 
        "(func() " <> prettyType t <+> "{" <> Doc.hardline <>
            indent (prettyBlock block) <> Doc.hardline <>
        "})()"

    BinOpExpr _ op lhs rhs ->
        prettyExpr lhs <+> prettyBinOp op <+> prettyExpr rhs

    UnaryOpExpr _ op x ->
        prettyUnaryOp op <> prettyExpr x

    StructAccessorExpr _ x field ->
        prettyExpr x <> Doc.dot <> prettyVisible prettyFieldName field

    SliceIndexerExpr _ x y ->
        prettyExpr x <> Doc.brackets (prettyExpr y)

    NilExpr _ -> 
        "nil"

    TypeAssertExpr t x -> 
        prettyExpr x <> Doc.dot <> Doc.parens (prettyType t)


prettyLiteral :: forall a. Literal Type -> Doc a
prettyLiteral = \case
    IntLiteral integer -> 
        Doc.pretty integer

    FloatLiteral double -> 
        Doc.pretty double

    StringLiteral str -> 
        Doc.dquotes (prettyPSString str)

    RuneLiteral c -> 
        Doc.squotes (Doc.pretty c)

    BoolLiteral b -> 
        if b then "true" else "false"

    SliceLiteral vs -> 
        let t = the (extractAnn <$> vs)
         in "[]" <> prettyType t <> 
             Doc.encloseSep Doc.lbrace Doc.rbrace (Doc.comma <> Doc.space) 
            (prettyExpr <$> vs)

    StructLiteral [] -> 
        "struct{}{}"

    StructLiteral fs -> 
        prettyType (StructType (bimap id extractAnn <$> fs)) <> "{" <> Doc.hardline <>
            indent (Doc.vsep (prettyField <$> fs)) <> Doc.hardline <>
        "}"

    NamedStructLiteral n [] -> 
        prettyQualified prettyTypeName n <> "{}" 

    NamedStructLiteral n fs -> 
        prettyQualified prettyTypeName n <> "{" <> Doc.hardline <> 
            indent (Doc.vsep (prettyField <$> fs)) <> Doc.hardline <>
        "}"
  where
    prettyField :: (Go.Visible Go.FieldName, Expr Type) -> Doc a
    prettyField (k, x) = 
        -- NOTE: When split over multiple lines, all struct values /must/ 
        -- have a trailing comma
        prettyVisible prettyFieldName k <> Doc.colon <+> prettyExpr x <> Doc.comma


prettyBlock :: Block Type -> Doc a
prettyBlock = \case
    ReturnStmnt e -> 
        "return" <+> prettyExpr e 

    PanicStmnt _ why -> 
        "panic" <> Doc.parens (Doc.dquotes (Doc.pretty why)) 

    VarStmnt t var what@FuncExpr{} r -> 
        -- In order to use a func recursively we need to declare it before
        -- we define it
        "var" <+> prettyVar var <+> prettyType t <> Doc.hardline <>
            prettyVar var <=> prettyExpr what <> Doc.hardline <> prettyBlock r

    VarStmnt t var what r -> 
        "var" <+> prettyVar var <+> prettyType t <=> prettyExpr what <> Doc.hardline <> prettyBlock r

    ConstStmnt t var what r -> 
        "const" <+> prettyVar var <+> prettyType t <=> prettyExpr what <> Doc.hardline <> prettyBlock r

    IfElseStmnt cond yes no ->
        "if" <+> prettyExpr cond <+> Doc.lbrace <> Doc.hardline <>
            indent (prettyBlock yes) <> Doc.hardline <>
            Doc.rbrace <> Doc.hardline <>
            prettyBlock no


prettyBinOp :: BinOp -> Doc a
prettyBinOp = \case
    AndOp -> "&&"
    EqOp -> "=="
    NotEqOp -> "!="


prettyUnaryOp :: UnaryOp -> Doc a
prettyUnaryOp = \case
    ReferenceOp   -> "&"
    DereferenceOp -> "*"


prettyType :: Type -> Doc a
prettyType = \case
    FuncType x y       -> "func(" <> prettyType x <> ")" <+> prettyType y
    IntType            -> "int"
    Float64Type        -> "float64"
    StringType         -> "string"
    RuneType           -> "rune"
    BoolType           -> "bool"
    SliceType v        -> "[]" <> prettyType v
    MapType k v        -> "map[" <> prettyType k <> "]" <> prettyType v
    NamedType n        -> prettyQualified prettyTypeName n
    PointerType t      -> "*" <> prettyType t
    TVar _             -> "interface{}"
    NilType t          -> prettyType t

    StructType [] -> "struct{}"
    StructType fs -> 
        "struct {" <> Doc.hardline <> 
            indent (Doc.vsep (prettyField <$> fs)) <> Doc.hardline <>
        "}"
  where
    prettyField :: (Go.Visible Go.FieldName, Type) -> Doc a
    prettyField (k, t) = prettyVisible prettyFieldName k <+> prettyType t


-- | @func foo(x int) int { ... }@
prettyFunc :: Maybe Go.Ident -> Type -> Go.Var -> Block Type -> Doc a
prettyFunc name (FuncType x y) var body = 
    prettySignature <+> "{" <> Doc.hardline <> 
        indent (prettyBlock body) <> Doc.hardline <>
    "}"
  where
    prettySignature :: Doc a
    prettySignature =
        "func" <> 
        maybe mempty ((Doc.space <>) . prettyGoIdent) name <> 
        Doc.parens (prettyVar var <+> prettyType x) <+> 
        prettyType y 

prettyFunc name wat _ _ = internalError $ 
    "Language.Go.Printer.prettyFunc: expecting FuncType, got " <> 
    show wat <> maybe "" (("\nFunction named: "<>) . show) name


-- NAMES


prettyVar :: Go.Var -> Doc a
prettyVar = prettyQualified prettyGoIdent


prettyQualified :: (a -> Doc b) -> Go.Qualified a -> Doc b
prettyQualified f (Go.Qualified Nothing a) = f a
prettyQualified f (Go.Qualified (Just pn) a) = 
    prettyPackageName pn <> Doc.dot <> f a


prettyVisible :: (a -> Doc b) -> Go.Visible a -> Doc b
prettyVisible f (Go.Public a)  = C.publicPrefix  <> f a
prettyVisible f (Go.Private a) = C.privatePrefix <> f a
prettyVisible f (Go.Local a)   = f a


prettyGoIdent :: Go.Ident -> Doc a
prettyGoIdent = prettyVisible prettyPSIdent


prettyTypeName :: Go.TypeName -> Doc a
prettyTypeName (Go.TypeName (PS.ProperName tn)) = Doc.pretty tn <> C.typeSuffix
prettyTypeName (Go.ClassName (PS.ProperName cn)) = Doc.pretty cn <> C.classSuffix


prettyConstructorName :: PS.ProperName 'PS.ConstructorName -> Doc a
prettyConstructorName = Doc.pretty . PS.runProperName


prettyPSIdent :: PS.Ident -> Doc a
prettyPSIdent (PS.Ident text) = Doc.pretty text
prettyPSIdent PS.UnusedIdent  = "_"
prettyPSIdent (PS.GenIdent mbtext i) = 
    "__" <> maybe mempty Doc.pretty mbtext <> Doc.pretty i


prettyPSString :: PSString -> Doc a
prettyPSString ps = maybe "???" Doc.pretty (decodeString ps) 


prettyFieldName :: Go.FieldName -> Doc a
prettyFieldName (Go.IdentField ident)      = prettyPSIdent ident
prettyFieldName (Go.StringField str)       = prettyPSString str
prettyFieldName (Go.ConstructorField ctor) = prettyConstructorName ctor
prettyFieldName (Go.LabelField label)      = prettyPSString (PS.runLabel label)


-- TRAVERSALS 


declTypes :: forall f. Applicative f => (Type -> f Type) -> Decl Type -> f (Decl Type)
declTypes f = \case
    VarDecl t ident x -> f t <&> (\t' -> VarDecl t' ident x)
    ConstDecl t ident x -> f t <&> (\t' -> ConstDecl t' ident x)
    TypeDecl tn t -> TypeDecl tn <$> f t
    FuncDecl t ident arg body -> 
        FuncDecl <$> f t <*> pure ident <*> pure arg <*> blockTypes f body


blockTypes :: Applicative f => (Type -> f Type) -> Block Type -> f (Block Type)
blockTypes f = \case
    ReturnStmnt x -> ReturnStmnt <$> exprTypes f x
    PanicStmnt t why -> flip PanicStmnt why <$> f t
    VarStmnt t var x y -> VarStmnt <$> f t <*> pure var <*> exprTypes f x <*> blockTypes f y
    ConstStmnt t var x y -> ConstStmnt <$> f t <*> pure var <*> exprTypes f x <*> blockTypes f y
    IfElseStmnt p x y -> IfElseStmnt <$> exprTypes f p <*> blockTypes f x <*> blockTypes f y


exprTypes :: Applicative f => (Type -> f Type) -> Expr Type -> f (Expr Type)
exprTypes f = \case
    VarExpr t var -> flip VarExpr var <$> f t
    LiteralExpr t lit -> LiteralExpr <$> f t <*> literalTypes f lit
    AppExpr t x y -> AppExpr <$> f t <*> exprTypes f x <*> exprTypes f y
    FuncExpr t arg body -> FuncExpr <$> f t <*> pure arg <*> blockTypes f body
    BlockExpr t block -> BlockExpr <$> f t <*> blockTypes f block
    BinOpExpr t op lhs rhs -> BinOpExpr <$> f t <*> pure op <*> exprTypes f lhs <*> exprTypes f rhs
    UnaryOpExpr t op x -> UnaryOpExpr <$> f t <*> pure op <*> exprTypes f x
    StructAccessorExpr t x field -> StructAccessorExpr <$> f t <*> pure x <*> pure field
    SliceIndexerExpr t x y -> SliceIndexerExpr <$> f t <*> pure x <*> pure y
    NilExpr t -> NilExpr <$> f t 
    TypeAssertExpr t x -> flip TypeAssertExpr x <$> f t 


literalTypes :: Applicative f => (Type -> f Type) -> Literal Type -> f (Literal Type)
literalTypes f lit = case lit of
    IntLiteral{} -> pure lit
    FloatLiteral{} -> pure lit
    BoolLiteral{} -> pure lit
    StringLiteral{} -> pure lit
    RuneLiteral{} -> pure lit
    SliceLiteral vs -> SliceLiteral <$> traverse (exprTypes f) vs
    StructLiteral fs -> StructLiteral <$> traverse (traverse (exprTypes f)) fs
    NamedStructLiteral tn fs -> NamedStructLiteral tn <$> traverse (traverse (exprTypes f)) fs


-- HELPERS


-- | Assignment 'Doc'.
--
-- > "x = y"
(<=>) :: Doc a -> Doc a -> Doc a
(<=>) x y = x <+> "=" <+> y


indent :: Doc a -> Doc a
indent = Doc.indent 4


hardStack :: [Doc a] -> Doc a
hardStack = Doc.concatWith (\x y -> x <> Doc.hardline <> y) 
