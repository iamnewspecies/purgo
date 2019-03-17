{-# LANGUAGE LambdaCase #-}
module Language.Go.AST.Traversals 
    ( everywhereOnValues
    ) 
where

import Prelude

import Language.Go.AST


everywhereOnValues
    :: (Decl a  -> Decl a)
    -> (Block a -> Block a)
    -> (Expr a  -> Expr a)
    -> (Decl a -> Decl a, Block a -> Block a, Expr a -> Expr a)
everywhereOnValues f g h = (f', g', h')
  where
    f' = \case
        VarDecl a ident x -> f (VarDecl a ident (h' x))
        ConstDecl a ident x -> f (ConstDecl a ident (h' x))
        FuncDecl a name arg block -> f (FuncDecl a name arg (g' block))
        TypeDecl tn t -> f (TypeDecl tn t)

    g' = \case
        ReturnStmnt x -> g (ReturnStmnt (h' x))
        PanicStmnt a why -> g (PanicStmnt a why)
        VarStmnt var t x block -> g (VarStmnt var t (h' x) (g' block))
        ConstStmnt var t x block -> g (ConstStmnt var t (h' x) (g' block))
        IfElseStmnt cond yes no -> g (IfElseStmnt (h' cond) (g' yes) (g' no))

    h' = \case
        VarExpr a var -> h (VarExpr a var)
        AppExpr a x y -> h (AppExpr a (h' x) (h' y))
        FuncExpr a arg block -> h (FuncExpr a arg (g' block))
        LiteralExpr a lit -> h (LiteralExpr a (handleLiteral lit))
        BlockExpr a block -> h (BlockExpr a (g' block))
        BinOpExpr a op lhs rhs -> h (BinOpExpr a op (h' lhs) (h' rhs))
        UnaryOpExpr a op x -> h (UnaryOpExpr a op (h' x))
        StructAccessorExpr a x name -> h (StructAccessorExpr a (h' x) name)
        SliceIndexerExpr a x y -> h (SliceIndexerExpr a (h' x) (h' y))
        NilExpr a -> h (NilExpr a)
        TypeAssertExpr a x -> h (TypeAssertExpr a (h' x))

    handleLiteral = \case 
        SliceLiteral vs -> SliceLiteral (fmap h' vs)
        StructLiteral fs -> StructLiteral (fmap (fmap h') fs)
        NamedStructLiteral tn fs -> NamedStructLiteral tn (fmap (fmap h') fs)

        lit@IntLiteral{}    -> lit
        lit@FloatLiteral{}  -> lit
        lit@StringLiteral{} -> lit
        lit@RuneLiteral{}   -> lit
        lit@BoolLiteral{}   -> lit
