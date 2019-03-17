{-# OPTIONS -fno-warn-missing-local-signatures #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Go.Tidy
    ( tidyDecl
    )
where

import Prelude

import Control.Arrow ((>>>))

import Language.Go.AST
import Language.Go.AST.Traversals (everywhereOnValues)


tidyDecl :: Eq a => Decl a -> Decl a
tidyDecl = tidyDeclFully


tidyDeclFully :: Eq a => Decl a -> Decl a
tidyDeclFully x | tidied == x = tidied
                    | otherwise      = tidyDeclFully tidied
  where tidied = tidyDeclPass x


tidyDeclPass :: Decl a -> Decl a
tidyDeclPass = tidy where
    (tidy, _, _) = everywhereOnValues
        -- Declarations
        (preferConstDecl >>> fixFuncDecls)

        -- Blocks
        (eliminateBlockExpressions >>> removeUnreachable)

        -- Expressions
        simplifyAnd

    preferConstDecl = \case
        VarDecl a ident x | canConst x -> ConstDecl a ident x
        decl -> decl
    
    fixFuncDecls = \case
        VarDecl a ident (FuncExpr _ arg block) ->
            FuncDecl a ident arg block
        decl -> decl

    eliminateBlockExpressions = \case
        ReturnStmnt (BlockExpr _ block) -> block
        block -> block
    
    removeUnreachable = \case
        IfElseStmnt GoTrue yes _ -> yes
        block -> block

    simplifyAnd = \case
        BinOpExpr _ AndOp GoTrue x -> x
        BinOpExpr _ AndOp x GoTrue -> x
        expr -> expr


-- | Returns whether the given expression is compatible with a  @const@ 
-- declaration/assignment.
canConst :: Expr a -> Bool
canConst = \case
    LiteralExpr _ IntLiteral{}    -> True
    LiteralExpr _ FloatLiteral{}  -> True
    LiteralExpr _ BoolLiteral{}   -> True
    LiteralExpr _ StringLiteral{} -> True
    LiteralExpr _ RuneLiteral{}   -> True
    _ -> False


pattern GoTrue :: Expr a
pattern GoTrue <- LiteralExpr _ (BoolLiteral True)
