{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Go.Compile
    ( compileFile
    , CompileError(..)
    )
where

import Prelude hiding (and)

import Control.Arrow ((>>>))
import Control.Monad (zipWithM)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, runReaderT)
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Language.PureScript.Constants (primModules)

import Language.Go.Crash (impossible)

import qualified Control.Monad.Reader as R
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.PureScript.AST.Literals as PS
import qualified Language.PureScript.AST.SourcePos as PS
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.Names as PS
import qualified System.FilePath.Posix as Posix

import qualified Language.Go.Ann as Go
import qualified Language.Go.AST as Go
import qualified Language.Go.AST.Traversals as Go (everywhereOnValues)
import qualified Language.Go.Compile.Assert as Go (assertBlock, assertExpr)
import qualified Language.Go.Environment as Go
import qualified Language.Go.File as Go
import qualified Language.Go.Names as Go
import qualified Language.Go.Tidy as Go (tidyDecl)
import qualified Language.Go.Types as Go


-- | Compile a Go source 'File' from parts of a 'CoreFn.Module'.
compileFile 
    :: MonadError CompileError m 
    => Go.Environment
    -> FilePath                  -- ^ import base (e.g. go mod/output)
    -> PS.ModuleName             -- ^ moduleName
    -> [(a, PS.ModuleName)]      -- ^ moduleImports 
    -> [PS.Ident]                -- ^ moduleExports
    -> [CF.Bind Go.Ann]          -- ^ moduleDecls
    -> m Go.File
compileFile env base mn imports exports binds = do
    let filePackageName = Go.PackageName mn
    let fileImports = compileImports base mn binds imports
    fileDecls <- runReaderT (compileDecls binds) (mn, env, Exports exports)
    pure Go.File{..}


compileImports 
    :: FilePath -> PS.ModuleName -> [CF.Bind Go.Ann] -> [(b, PS.ModuleName)] 
    -> [Go.Import]
compileImports base currentModule binds = 
        fmap snd 
    >>> filter (`notElem` primModules)    -- Prim is implicit
    >>> filter (/= currentModule)          
    >>> filter (`S.member` usedImports)   -- remove redundant imports
    >>> fmap mkImport
  where
    mkImport :: PS.ModuleName -> Go.Import
    mkImport mn = Go.Import
        { importName = Just (Go.PackageName mn)
        , importPath = Posix.joinPath (base : [T.unpack (PS.runModuleName mn)])
        }

    usedImports :: S.Set PS.ModuleName
    usedImports = S.unions (extractUsed <$> binds) where
        extractUsed :: CF.Bind Go.Ann -> S.Set PS.ModuleName
        (extractUsed, _, _, _) = CF.everythingOnValues S.union
            (const S.empty)  -- Bind
            (\case 
                CF.Var _ (PS.Qualified (Just mn) _) -> 
                    S.singleton mn

                expr -> case Go.annType (CF.extractAnn expr) of
                    Go.NamedType (Go.Qualified pn _) -> 
                        maybe S.empty (S.singleton . Go.unPackageName) pn
                    _                               -> S.empty

            )
            (const S.empty)  -- Binder
            (const S.empty)  -- CaseAlternative


type MonadCompile m =
    ( MonadError CompileError m
    , MonadReader (PS.ModuleName, Go.Environment, Exports) m
    )


asksIsExported :: MonadCompile m => PS.Ident -> m Bool
asksIsExported ident = R.asks (\(_, _, exports) -> isExported ident exports)


asksModuleName :: MonadCompile m => m PS.ModuleName
asksModuleName = R.asks (\(mn, _, _) -> mn)


-- | Reasons that compilation could fail.
data CompileError 
    = TypeError
    | UnImplemented (CF.Expr ())
    deriving (Show)


throwUnimplemented :: MonadError CompileError m => CF.Expr a -> m b
throwUnimplemented e = throwError $ UnImplemented (() <$ e)


-- | Identifiers exported by a module.
newtype Exports = Exports [PS.Ident]


isExported :: PS.Ident -> Exports -> Bool
isExported ident (Exports exports) = ident `elem` exports


constructorIsExported :: PS.ProperName 'PS.ConstructorName -> Exports -> Bool
constructorIsExported cn = isExported (PS.Ident (PS.runProperName cn))


compileDecls :: MonadCompile m => [CF.Bind Go.Ann] -> m [Go.Decl Go.Type]
compileDecls = fmap (fmap Go.tidyDecl) . go 
  where
    go :: MonadCompile m => [CF.Bind Go.Ann] -> m [Go.Decl Go.Type]
    go [] = pure []
    go (CF.NonRec t ident x : rest) =
        (:) <$> compileDecl t ident x <*> go rest
    go (CF.Rec rec : rest) =
        -- Flattening out recursive binding groups because the Go compiler 
        -- doesn't care (I don't think...)
        (<>) <$> traverse (uncurry (uncurry compileDecl)) rec <*> go rest


compileDecl 
    :: MonadCompile m
    => Go.Ann 
    -> PS.Ident 
    -> CF.Expr Go.Ann 
    -> m (Go.Decl Go.Type)
compileDecl ann ident' expr' = do
    exported <- asksIsExported ident'
    let ident = if exported then Go.Public ident' else Go.Private ident'
    let t = Go.annType ann
    compileExpr expr' >>= \case
        -- Convert function expressions to func declarations at the top-level
        Go.FuncExpr _ arg block -> 
            case t of
                Go.FuncType _ want ->
                    pure $ Go.FuncDecl t ident arg (Go.assertBlock want block)
                _ -> 
                    impossible

        -- Otherwise create a var binding 
        -- (optimized to a const later)
        expr -> pure $ Go.VarDecl t ident (Go.assertExpr t expr)


compileExpr :: MonadCompile m => CF.Expr Go.Ann -> m (Go.Expr Go.Type)
compileExpr = \case
    CF.Literal ann lit -> 
        Go.LiteralExpr (Go.annType ann) <$> compileExprLiteral lit

    CF.Abs ann arg x' -> 
        Go.FuncExpr (Go.annType ann) (localVar arg) . Go.ReturnStmnt <$> compileExpr x'

    CF.App ann f' x' -> 
        Go.AppExpr (Go.annType ann) <$> compileExpr f' <*> compileExpr x'

    CF.Var ann var -> 
        Go.VarExpr (Go.annType ann) <$> compileQualifiedIdent var

    CF.Constructor ann tn cn idents ->
        compileConstructor tn cn (Go.unFunc . Go.annType $ ann) idents

    CF.Case ann xs' cas' -> 
        Go.BlockExpr (Go.annType ann) <$> compileCase ann xs' cas'

    expr -> throwUnimplemented expr


compileConstructor 
    :: forall m. MonadCompile m 
    => PS.ProperName 'PS.TypeName
    -> PS.ProperName 'PS.ConstructorName
    -> [Go.Type]
    -> [PS.Ident]
    -> m (Go.Expr Go.Type)
compileConstructor tn' cn ts idents = R.asks (`go` zipped) where
    zipped :: [(PS.Ident, Go.Type)]
    zipped = zip idents ts -- NOTE: zipping will drop the final return type

    go :: (PS.ModuleName, a, Exports) -> [(PS.Ident, Go.Type)] -> Go.Expr Go.Type
    go r@(mn, _, exports) = \case
        [] -> 
            let pn = Go.PackageName mn
                tn = Go.TypeName tn'
             in Go.LiteralExpr (Go.NamedType (Go.Qualified (Just pn) tn)) $
                -- NOTE: We shouldn't qualify the named struct literal because
                -- this will only ever be compiled in the module defining the
                -- type
                Go.NamedStructLiteral (Go.Qualified Nothing tn)
                    [ ( visible (Go.ConstructorField cn)
                      , Go.UnaryOpExpr (Go.PointerType fieldType) Go.ReferenceOp $
                            Go.LiteralExpr fieldType $
                                Go.StructLiteral (uncurry mkField <$> zipped)
                      )
                    ]
        ((ident, x) : rest) ->
            let result = go r rest; y = Go.extractAnn result
             in Go.FuncExpr (Go.FuncType x y) 
                            (localVar ident) 
                            (Go.ReturnStmnt result)
      where
        visible :: a -> Go.Visible a
        visible = 
            if constructorIsExported cn exports 
               then Go.Public 
               else Go.Private

        fieldType :: Go.Type
        fieldType = 
            Go.StructType (bimap (Go.Private . Go.IdentField) id <$> zipped)

        mkField :: PS.Ident -> Go.Type -> (Go.Field, Go.Expr Go.Type)
        mkField ident t = 
            (visible (Go.IdentField ident), Go.VarExpr t (localVar ident))
        

compileExprLiteral 
    :: MonadCompile m 
    => PS.Literal (CF.Expr Go.Ann) 
    -> m (Go.Literal Go.Type)
compileExprLiteral = \case
    PS.NumericLiteral (Left integer) -> 
        pure (Go.IntLiteral integer)

    PS.NumericLiteral (Right double) -> 
        pure (Go.FloatLiteral double)

    PS.StringLiteral psString -> 
        pure (Go.StringLiteral psString)

    PS.BooleanLiteral b -> 
        pure (Go.BoolLiteral b)

    PS.CharLiteral c -> 
        pure (Go.RuneLiteral c)

    PS.ArrayLiteral vs -> 
        Go.SliceLiteral <$> traverse compileExpr vs

    PS.ObjectLiteral obj -> do
        let (ks', vs') = unzip obj
        let ks = Go.Private . Go.StringField <$> ks'
        vs <- traverse compileExpr vs'
        pure (Go.StructLiteral (zip ks vs))


type Condition a = Go.Expr a -- for clarity


compileCase 
    :: MonadCompile m 
    => Go.Ann
    -> [CF.Expr Go.Ann]
    -> [CF.CaseAlternative Go.Ann]
    -> m (Go.Block Go.Type)
compileCase ann _ [] = do
    (mn, _, _) <- R.ask
    let err = "Failed pattern match at " <> PS.runModuleName mn <> " " <>
              PS.displayStartEndPos (Go.annSourceSpan ann)
    pure (Go.PanicStmnt (Go.annType ann) err)
compileCase ann xs (ca : rest') = do
    ifs  <- compileCaseAlternative xs ca
    rest <- compileCase ann xs rest'
    pure $ foldr (\(conds, yes) -> Go.IfElseStmnt (and conds) yes) rest ifs
  where
    and :: [Condition Go.Type] -> Condition Go.Type
    -- NOTE: This first case should ultimately be optimized away
    and []       = Go.LiteralExpr Go.BoolType (Go.BoolLiteral True) 
    and (c : cs) = foldl (Go.BinOpExpr Go.BoolType Go.AndOp) c cs -- foldl or foldr?


compileCaseAlternative 
    :: MonadCompile m
    => [CF.Expr Go.Ann]
    -> CF.CaseAlternative Go.Ann
    -> m [([Go.Expr Go.Type], Go.Block Go.Type)]
compileCaseAlternative patterns' CF.CaseAlternative{..} = do
    patterns <- traverse compileExpr patterns'
    (conds, subs) <- mconcat <$> zipWithM compileBinder patterns caseAlternativeBinders
    case caseAlternativeResult of
        Left guards ->
            for guards $ \(guard', result') -> do
                guard <- substituteVars subs <$> compileExpr guard'
                result <- substituteVars subs <$> compileExpr result'
                pure (conds <> [guard], Go.ReturnStmnt result) 

        Right result' -> do
            result <- substituteVars subs <$> compileExpr result'
            pure [(conds, Go.ReturnStmnt result)]
  where
    substituteVars 
        :: M.Map Go.Ident (Go.Expr Go.Type)
        -> Go.Expr Go.Type
        -> Go.Expr Go.Type
    substituteVars subs = substitute where
        (_, _, substitute) = Go.everywhereOnValues id id $ 
            \expr -> case expr of
                Go.VarExpr _t (Go.Qualified Nothing ident) -> 
                    fromMaybe expr (M.lookup ident subs)
                _ -> expr


-- | Compile a binder (used for case patterns) to @if@ conditions and identifier
-- substitutions. 
compileBinder 
    :: MonadCompile m 
    => Go.Expr Go.Type
    -> CF.Binder Go.Ann 
    -> m ([Condition Go.Type], M.Map Go.Ident (Go.Expr Go.Type))
compileBinder x = \case
    CF.NullBinder _ ->
        pure mempty

    CF.VarBinder _ ident -> 
        pure ([], M.singleton (Go.Local ident) x)
    
    CF.ConstructorBinder _ tn cn' binders -> do
        let cn = PS.disqualify cn'
        -- TODO: Handle the error here
        (mn, env, exports) <- R.ask
        fields <- maybe undefined pure (Go.lookupConstructorFields tn cn env)

        let visible :: a -> Go.Visible a
            visible = maybe Go.Private 
                (\q -> if | q /= mn -> Go.Public
                          | constructorIsExported cn exports -> Go.Public
                          | otherwise -> Go.Private
                )
                (PS.getQual cn')
        
        -- Check that this constructor isn't nil
        let ptr = Go.PointerType (Go.StructType fields)
        let ctor = Go.StructAccessorExpr ptr x (visible (Go.ConstructorField cn))
        let nil = Go.NilExpr (Go.NilType ptr)
        let ctorCheck = Go.BinOpExpr Go.BoolType Go.NotEqOp ctor nil

        (conds, subs) <- mconcat <$> zipWithM compileBinder 
            (fmap (\(fn, t) -> Go.StructAccessorExpr t ctor fn) fields)
            binders

        pure (ctorCheck : conds, subs)

    CF.NamedBinder _ ident binder -> do
        (conds, subs) <- compileBinder x binder
        pure (conds, M.insert (Go.Local ident) x subs)

    CF.LiteralBinder _ (PS.NumericLiteral (Left integer)) -> do
        let want = Go.LiteralExpr Go.IntType (Go.IntLiteral integer)
        pure ([x `goEq` want], mempty) 

    CF.LiteralBinder _ (PS.NumericLiteral (Right double)) -> do
        let want = Go.LiteralExpr Go.Float64Type (Go.FloatLiteral double)
        pure ([x `goEq` want], mempty) 

    CF.LiteralBinder _ (PS.StringLiteral psString) -> do
        let want = Go.LiteralExpr Go.StringType (Go.StringLiteral psString)
        pure ([x `goEq` want], mempty) 

    CF.LiteralBinder _ (PS.CharLiteral char) -> do
        let want = Go.LiteralExpr Go.RuneType (Go.RuneLiteral char)
        pure ([x `goEq` want], mempty) 

    CF.LiteralBinder _ (PS.BooleanLiteral b) -> do
        let want = Go.LiteralExpr Go.BoolType (Go.BoolLiteral b)
        pure ([x `goEq` want], mempty) 

    CF.LiteralBinder _ (PS.ArrayLiteral bs) -> do
        -- NOTE: Length check shouldn't be necessary here
        let idxs = Go.LiteralExpr Go.IntType . Go.IntLiteral <$> [0..]
        mconcat <$> 
            zipWithM (compileBinder . Go.SliceIndexerExpr Go.IntType x) idxs bs

    CF.LiteralBinder _ (PS.ObjectLiteral _)   -> undefined
  where
    goEq :: Go.Expr Go.Type -> Go.Expr Go.Type -> Go.Expr Go.Type
    goEq = Go.BinOpExpr Go.BoolType Go.EqOp

        
compileQualifiedIdent 
    :: MonadCompile m => PS.Qualified PS.Ident -> m (Go.Qualified Go.Ident)
compileQualifiedIdent (PS.Qualified Nothing ident') = pure (localVar ident')
compileQualifiedIdent (PS.Qualified (Just mn) ident') = do
    let pn = Go.PackageName mn
    currentModule <- asksModuleName
    if mn /= currentModule 
       then pure (Go.Qualified (Just pn) (Go.Public ident'))
       else do exported <- asksIsExported ident'
               let ident = if exported then Go.Public ident' else Go.Private ident'
               pure (Go.Qualified Nothing ident)


localVar :: PS.Ident -> Go.Var
localVar = Go.Qualified Nothing . Go.Local
