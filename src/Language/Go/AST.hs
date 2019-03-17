{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
module Language.Go.AST
    ( Import(..)
    , Decl(..)
    , Block(..)
    , Expr(..)
    , extractAnn
    , Literal(..)
    , BinOp(..)
    , UnaryOp(..)
    )
where

import Prelude

import Data.Text (Text)
import Language.PureScript.PSString (PSString)

import Language.Go.Types (Type)

import qualified System.FilePath.Posix as Posix

import qualified Language.Go.Names as Go


-- | A Go import
--
-- > Import Nothing "fmt" == import "fmt"
-- > Import (Just foo) "bar/baz" == import foo "bar/baz"
data Import = Import
    { importName :: Maybe Go.PackageName
    , importPath :: Posix.FilePath
    }


-- | Top-level Go declaration.
data Decl a
    = VarDecl a Go.Ident (Expr a)
    | ConstDecl a Go.Ident (Expr a)
    | FuncDecl a Go.Ident Go.Var (Block a)
    | TypeDecl Go.TypeName Type
    deriving (Show, Eq)


-- | Block of Go statements. 
data Block a
    = ReturnStmnt (Expr a)
    | PanicStmnt a Text
    | VarStmnt a Go.Var (Expr a) (Block a)
    | ConstStmnt a Go.Var (Expr a) (Block a)
    | IfElseStmnt (Expr a) (Block a) (Block a)
    deriving (Show, Eq, Functor)


-- | Go expression.
data Expr a 
    = VarExpr a Go.Var
    | AppExpr a (Expr a) (Expr a)
    | FuncExpr a Go.Var (Block a)
    | LiteralExpr a (Literal a)
    | BlockExpr a (Block a)
    | BinOpExpr a !BinOp (Expr a) (Expr a)
    | UnaryOpExpr a !UnaryOp (Expr a)
    | StructAccessorExpr a (Expr a) Go.Field
    | SliceIndexerExpr a (Expr a) (Expr a)
    | NilExpr a
    | TypeAssertExpr a (Expr a)
    deriving (Show, Eq, Functor)


extractAnn :: Expr a -> a
extractAnn = \case
    VarExpr a _ -> a
    AppExpr a _ _ -> a
    FuncExpr a _ _ -> a
    LiteralExpr a _ -> a
    BlockExpr a _ -> a
    BinOpExpr a _ _ _ -> a
    UnaryOpExpr a _ _ -> a
    StructAccessorExpr a _ _ -> a
    SliceIndexerExpr a _ _ -> a
    NilExpr a -> a
    TypeAssertExpr a _ -> a


-- | Go literal.
data Literal a
    = IntLiteral !Integer
    | FloatLiteral !Double
    | StringLiteral PSString
    | RuneLiteral !Char
    | BoolLiteral !Bool
    | SliceLiteral [Expr a]
    | StructLiteral [(Go.Field, Expr a)]
    | NamedStructLiteral (Go.Qualified Go.TypeName) [(Go.Field, Expr a)]
    deriving (Show, Eq, Functor)


-- | Binary operators.
data BinOp 
    = AndOp
    | EqOp
    | NotEqOp
    deriving (Show, Eq)


-- | Unary operators.
data UnaryOp 
    = ReferenceOp
    | DereferenceOp
    deriving (Show, Eq)
