{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Make all the types fit together.
module Language.Go.Compile.Assert 
    ( assertBlock
    , assertExpr
    )
where

import Prelude

import Data.Foldable (foldl')

import Language.Go.AST
import Language.Go.Crash (internalError)
import Language.Go.Types (pattern EmptyInterfaceType, Type(..))

import qualified Data.Text as T

import qualified Language.Go.Names as Go
import qualified Language.PureScript.Names as PS


assertBlock :: Type -> Block Type -> Block Type 
assertBlock want = \case
    ReturnStmnt x -> 
        ReturnStmnt (assertExpr want x)

    panic@PanicStmnt{} -> 
        panic

    VarStmnt t var x b -> 
        VarStmnt t var (assertExpr t x) (assertBlock want b)

    ConstStmnt t var x b -> 
        ConstStmnt t var (assertExpr t x) (assertBlock want b)

    IfElseStmnt cond yes no ->
        IfElseStmnt (assertExpr BoolType cond) 
            (assertBlock want yes) 
            (assertBlock want no)


assertExpr :: Type -> Expr Type -> Expr Type
assertExpr want = \case
    var@(VarExpr got _) ->
        typeAssert want got var

    AppExpr _got f x ->
        case extractAnn f of
            FuncType expects got ->
                -- New problem: We're asserting to what we /want/ when we should
                -- really be asserting to what we know we /have/.
                typeAssert want got $ AppExpr got
                    (assertExpr (FuncType expects got) f) 
                    (assertExpr expects x)
                
            _ ->
                internalError "wtf"

    FuncExpr got arg body -> 
        case got of 
            FuncType _ returns ->
                typeAssert want got $
                    FuncExpr got arg (assertBlock returns body)

            _ -> 
                internalError "wtf"

    BinOpExpr got AndOp x y 
        | want /= BoolType -> internalError "wtf"
        | got /= BoolType -> internalError "wtf"
        | otherwise -> 
            BinOpExpr BoolType AndOp 
                (assertExpr BoolType x) 
                (assertExpr BoolType y)

    BinOpExpr got EqOp x y 
        | want /= BoolType -> internalError "wtf"
        | got /= BoolType -> internalError "wtf"
        | otherwise -> 
            uncurry (BinOpExpr BoolType EqOp) (assertSame x y)

    BinOpExpr got NotEqOp x y 
        | want /= BoolType -> internalError "wtf"
        | got /= BoolType -> internalError "wtf"
        | otherwise -> 
            uncurry (BinOpExpr BoolType EqOp) (assertSame x y)

    BlockExpr got block ->
        typeAssert want got (BlockExpr got (assertBlock got block))

    nil@(NilExpr got) ->
        typeAssert want got nil

    assertion@(TypeAssertExpr got _) ->
        typeAssert want got assertion

    todo@LiteralExpr{} -> todo
    todo@UnaryOpExpr{} -> todo
    todo@StructAccessorExpr{} -> todo
    todo@SliceIndexerExpr{} -> todo


assertSame :: Expr Type -> Expr Type -> (Expr Type, Expr Type)
assertSame x y = case (extractAnn x, extractAnn y) of 
    (EmptyInterfaceType, EmptyInterfaceType) -> (x, y)
    (other, EmptyInterfaceType)              -> (x, assertExpr other y)
    (EmptyInterfaceType, other)              -> (assertExpr other x, y)
    _                                        -> (x, y)


typeAssert :: Type -> Type -> Expr Type -> Expr Type
typeAssert want got x 
    | want `equiv` got = x -- NoOp
    | otherwise = case (want, got) of
        (FuncType{}, FuncType{}) -> reFunc [] want got x
        -- Can only assert interface{} types
        (_, EmptyInterfaceType)  -> TypeAssertExpr want x
        _                        -> x 


reFunc :: [(Type, Go.Var)] -> Type -> Type -> Expr Type -> Expr Type
reFunc vars want got x 
    | want `equiv` got = foldl' appExpr x (uncurry VarExpr <$> vars)
    | otherwise = case (want, got) of
        (FuncType expects want', FuncType _ got') ->
            FuncExpr want nextArg . ReturnStmnt $
                reFunc (vars <> [(expects, nextArg)]) want' got' x

        _ -> foldl' appExpr (typeAssert want got x) (uncurry VarExpr <$> vars)
  where
    nextArg :: Go.Var
    nextArg = mkArg (length vars)

    appExpr :: Expr Type -> Expr Type -> Expr Type
    appExpr f' x' = case extractAnn f' of
        FuncType _ returns -> assertExpr returns (AppExpr returns f' x')
        other              -> internalError ("wtf: " <> show other)

    mkArg :: Int -> Go.Var
    mkArg = Go.Qualified Nothing 
          . Go.Local 
          . PS.Ident 
          . T.pack 
          . ("v" <>) 
          . show


equiv :: Type -> Type -> Bool
FuncType a c  `equiv` FuncType b d  = a `equiv` b && c `equiv` d
IntType       `equiv` IntType       = True
Float64Type   `equiv` Float64Type   = True
StringType    `equiv` StringType    = True
RuneType      `equiv` RuneType      = True
BoolType      `equiv` BoolType      = True
SliceType a   `equiv` SliceType b   = a `equiv` b
MapType a c   `equiv` MapType b d   = a `equiv` b && c `equiv` d
PointerType a `equiv` PointerType b = a `equiv` b
NilType a     `equiv` NilType b     = a `equiv` b
StructType a  `equiv` StructType b  = and $
    zipWith (\(fn1, t1) (fn2, t2) -> fn1 == fn2 && t1 `equiv` t2) a b
NamedType a   `equiv` NamedType b   = a == b
TVar _        `equiv` TVar _        = True
_             `equiv` _             = False


-- unTypeAssert :: Expr a -> Expr a
-- unTypeAssert (TypeAssertExpr _ x) = unTypeAssert x
-- unTypeAssert x                    = x
