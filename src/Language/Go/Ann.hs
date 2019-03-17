{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
-- | https://github.com/purescript-go/purgo/blob/a1da15d2630723136adf62a496eca77ab2a0aa2e/src/Language/Go/Ann.hs
module Language.Go.Ann
    ( Ann(..)
    , TypeError(..)
    , annotateDecls
    )
where

import Prelude

import Control.Applicative ((<|>))
import Control.Monad (mapAndUnzipM, zipWithM)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.Supply.Class (MonadSupply)
import Data.Bifunctor (second)
import Data.Foldable (foldl')
import Data.Map (Map)
import Data.Set (Set)
import GHC.Exts (IsList(..))
import Language.PureScript.AST.Literals (Literal(..))
import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.CoreFn
    ( Bind(..)
    , Binder(..)
    , CaseAlternative(..)
    , Expr(..)
    , Guard
    )

import Language.Go.Orphans ()
import Language.Go.Types as Go

import qualified Control.Monad.Reader as R
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.Names as PS

import qualified Language.Go.Environment as Go
import qualified Language.Go.Names as Go


-- | Go annotation.
--
-- This is the annotation we need @CoreFn@ to have in order to compile it to Go.
data Ann = Ann
    { annSourceSpan :: SourceSpan
    , annType       :: Go.Type 
    }
    deriving (Show)


-- | Convert a core annotation to a Go annotation, given a Go type. 
--
-- If the core annotation has a type then we try and use it to generate 
-- a constraint.
mkAnn :: MonadSupply m => CF.Ann -> Go.Type -> m (Ann, [Constraint])
mkAnn (ss, _, Nothing, _) t = pure (Ann { annSourceSpan = ss, annType = t }, [])
mkAnn (ss, _, Just pst, _) t = runExceptT (Go.typeFromPS pst) >>= \case
    Left _unsupportedType -> 
        pure (Ann { annSourceSpan = ss, annType = t }, [])

    Right t' -> 
        pure (Ann { annSourceSpan = ss, annType = t }, [(t, t')])


-- | Add Go type annotations to a bunch of top-level declarations.
annotateDecls 
    :: (MonadSupply m, MonadError TypeError m) 
    => Go.Environment 
    -> PS.ModuleName 
    -> [Bind CF.Ann] 
    -> m [Bind Ann]
annotateDecls env mn decls' = 
    flip runReaderT (mn, env, mempty) $ do
        -- TODO: We need to be smarter here. 
        -- A non-recursive bind at the top-level shouldn't have it's type 
        -- determined by how it's used elsewhere.
        (decls, c) <- annotateBinds (Just mn) decls'
        s <- solve c
        pure (fmap (freezeAnn . applySubst s) <$> decls)
  where
    -- Once we've decided on a type we need to /lock it in/ which
    -- requires that we "freeze" all the type variables.
    freezeAnn :: Ann -> Ann
    freezeAnn ann = ann { annType = freezeTVars (annType ann) }


-- | Constraints have to be solved.
type Constraint = (Type, Type)


-- | Reasons that type inference could fail.
--
-- None of these shouldn't really happen as we /should/ be working with
-- @CoreFn@ generated by @purs@, which has already done far more sophisticated
-- checks. If one of these /does/ get thrown it's probably because I've done
-- something stupid.
data TypeError
    = CannotUnify Type Type
    | InfiniteType TVar Type  -- ^ occurs check failed
    | UnboundVariable Var
    | DodgyArray [Expr ()]
    | DodgyCase [CaseAlternative ()]
    | Unimplemented (Expr ()) -- XXX
    deriving (Show)


-- | Classy inference Monad.
type MonadInfer m = 
    ( MonadError TypeError m
    , MonadSupply m
    , MonadReader (PS.ModuleName, Go.Environment, TypeEnv) m
    )


-- | Modify the environment in which an inference action will run.
localTypeEnv :: MonadInfer m => (TypeEnv -> TypeEnv) -> m a -> m a
localTypeEnv f = R.local (\(mn, env, tenv) -> (mn, env, f tenv))


-- | Run an inference action in an extended type environment. 
inEnv :: MonadInfer m => (Var, Scheme) -> m a -> m a
inEnv = localTypeEnv . extendTypeEnv 


-- NOTE: We can't use Go names yet as we need visibility info for that.
type Var = PS.Qualified PS.Ident


-- | Create an unqualified (i.e. local) variable from a bare identifier.
localVar :: PS.Ident -> Var
localVar = PS.Qualified Nothing


-- | Type environment - i.e. all the types we know about.
newtype TypeEnv = TypeEnv { unTypeEnv :: Map Var Scheme }
    deriving stock (Show)


instance Semigroup TypeEnv where
    -- NOTE: This is left biased!
    TypeEnv x <> TypeEnv y = TypeEnv (M.union x y)


instance Monoid TypeEnv where
    mempty = TypeEnv M.empty


instance IsList TypeEnv where
    type Item TypeEnv = (Var, Scheme)
    fromList = TypeEnv . fromList
    toList = toList . unTypeEnv


lookupTypeEnv :: Var -> TypeEnv -> Maybe Scheme
lookupTypeEnv var (TypeEnv tenv) = M.lookup var tenv


-- | Extend the type environment.
extendTypeEnv :: (Var, Scheme) -> TypeEnv -> TypeEnv
extendTypeEnv (var, sc) (TypeEnv tenv) = 
    TypeEnv (M.insert var sc tenv)
    --TypeEnv (M.insert var sc (M.delete var tenv))  ???


-- | Extend the type environment with a generalized monotype.
extendTypeEnvGen :: (Var, Type) -> TypeEnv -> TypeEnv
extendTypeEnvGen (var, t) tenv = extendTypeEnv (var, generalize tenv t) tenv


-- | Polytype: a monotype universally quantified over a number of type variables.
data Scheme = Forall (Set TVar) Type
    deriving (Show)


-- | A substitution is a mapping from type variables to (mono)types.
newtype Subst = Subst { unSubst :: Map TVar Type }
    deriving newtype (Show)


instance Semigroup Subst where
    Subst s1 <> Subst s2 = 
        -- NOTE: M.union is left-biased
        Subst (M.map (applySubst (Subst s1)) s2 `M.union` s1)


instance Monoid Subst where
    mempty = Subst M.empty


instance IsList Subst where
    type Item Subst = (TVar, Type)
    fromList = Subst . fromList
    toList = toList . unSubst



-- ANNOTATING THINGS


annotateExpr :: MonadInfer m => Expr CF.Ann -> m (Expr Ann, [Constraint])
annotateExpr = \case
    Var a var -> do
        (_, env, tenv) <- R.ask
        p <- traverse instantiate (lookupTypeEnv var tenv)  -- polytype
        let m = M.lookup var (Go.envNames env)              -- monotype
        case p <|> m of 
            Nothing -> throwError (UnboundVariable var)
            Just t -> do
                (ann, c) <- mkAnn a t
                pure (Var ann var, c)
    
    Abs a ident x' -> do
        tv <- freshTVar -- argument type
        (x, c1) <- inEnv (localVar ident, Forall [] tv) (annotateExpr x')
        (ann, c2) <- mkAnn a (tv `FuncType` extractAnnType x)
        pure (Abs ann ident x, c1 <> c2)

    App a f' x' -> do
        (f, c1) <- annotateExpr f'
        (x, c2) <- annotateExpr x'
        tv <- freshTVar -- return type of f 
        let c3 = (extractAnnType f, extractAnnType x `FuncType` tv)
        (ann, c4) <- mkAnn a tv
        pure (App ann f x, c1 <> c2 <> [c3] <> c4)

    Let a binds' x'  -> do
        (binds_, c1) <- annotateBinds Nothing binds'

        -- TODO: Are these next two lines necessary at this stage?
        s <- solve c1
        -- I think 'c1' is done with now...?
        let binds = fmap (applySubst s) <$> binds_

        localTypeEnv (loadBinds binds) $ do
            (x, c2) <- annotateExpr x'
            (ann, c3) <- mkAnn a (extractAnnType x)
            pure (Let ann binds x, c1 <> c2 <> c3)

    Constructor a tn cn idents -> do
        (mn, env, _) <- R.ask
        let qtn = PS.Qualified (Just mn) tn
        fields <- maybe undefined pure (Go.lookupConstructorFields qtn cn env)
        let pn = Go.PackageName mn
        let t = Go.NamedType (Go.Qualified (Just pn) (Go.TypeName tn))
        (ann, c) <- mkAnn a (foldr (Go.FuncType . snd) t fields)
        pure (Constructor ann tn cn idents, c)
    
    Case a xs' cas' -> do
        (xs, c1) <- second mconcat <$> mapAndUnzipM annotateExpr xs'
        (cas, c2) <-  annotateCaseAlternatives (extractAnnType <$> xs)  cas'
        t <- freshTVar -- yeh?
        (ann, c3) <- mkAnn a t
        pure (Case ann xs cas, c1 <> c2 <> c3)

    Literal a (NumericLiteral (Left integer)) -> do
        (ann, c) <- mkAnn a IntType
        pure (Literal ann (NumericLiteral (Left integer)), c)

    Literal a (NumericLiteral (Right double)) -> do
        (ann, c) <- mkAnn a Float64Type
        pure (Literal ann (NumericLiteral (Right double)), c)

    Literal a (StringLiteral psString) -> do
        (ann, c) <- mkAnn a StringType
        pure (Literal ann (StringLiteral psString), c)

    Literal a (CharLiteral char) -> do
        (ann, c) <- mkAnn a RuneType
        pure (Literal ann (CharLiteral char), c)

    Literal a (BooleanLiteral bool) -> do
        (ann, c) <- mkAnn a BoolType
        pure (Literal ann (BooleanLiteral bool), c)

    other -> throwUnimplemented other


annotateBinds 
    :: forall m. MonadInfer m 
    => Maybe PS.ModuleName 
    -> [Bind CF.Ann] 
    -> m ([Bind Ann], [Constraint])
annotateBinds _ [] = pure mempty
annotateBinds mn (NonRec a ident x' : rest') = do
    (((ann, _), x), c1) <- annotateBind a ident x'
    inEnv (PS.Qualified mn ident, Forall [] (annType ann)) $ do
        (rest, c2) <- annotateBinds mn rest'
        pure (NonRec ann ident x : rest, c1 <> c2)

annotateBinds mn (Rec rec' : rest') = do
    let idents = fmap (snd . fst) rec'
    tenv <- TypeEnv . M.fromList <$> traverse initBind idents
    localTypeEnv (tenv <>) $ do 
        (rec, cs) <- mapAndUnzipM (uncurry (uncurry annotateBind)) rec'
        (rest, c) <- annotateBinds mn rest'
        pure (Rec rec : rest, mconcat cs <> c)
  where
    initBind :: PS.Ident -> m (Var, Scheme)
    initBind ident = (PS.Qualified mn ident,) . Forall [] <$> freshTVar


annotateBind
    :: MonadInfer m 
    => CF.Ann -> PS.Ident -> Expr CF.Ann 
    -> m (((Ann, PS.Ident), Expr Ann), [Constraint])
annotateBind a ident x' = do
    (x, c1) <- annotateExpr x'
    (ann, c2) <- mkAnn a (extractAnnType x)
    pure (((ann, ident), x), c1 <> c2)


-- | Load annotated bindings into the type environment.
loadBinds :: [Bind Ann] -> TypeEnv -> TypeEnv
loadBinds = flip (foldl' go)
  where
    go :: TypeEnv -> Bind Ann -> TypeEnv
    go tenv (NonRec ann ident _) = 
        extendTypeEnv (localVar ident, generalize tenv (annType ann))  tenv
    go tenv (Rec [])             = tenv
    go tenv (Rec (((ann, ident), _) : rest)) =
        go (extendTypeEnvGen (localVar ident, annType ann) tenv) (Rec rest)


annotateCaseAlternatives
    :: MonadInfer m => [Type]
    -> [CaseAlternative CF.Ann]
    -> m ([CaseAlternative Ann], [Constraint])
annotateCaseAlternatives _ [] = pure mempty
annotateCaseAlternatives ts (CaseAlternative{..} : rest') = do
    (cabs, tenvs, cs) <- unzip3 <$> 
        zipWithM annotateBinder ts caseAlternativeBinders
    (car, c2) <- localTypeEnv (mconcat tenvs <>) $ 
        annotateCaseResult caseAlternativeResult
    (rest, c3) <- annotateCaseAlternatives ts rest'
    pure (CaseAlternative cabs car : rest, mconcat cs <> c2 <> c3)


annotateCaseResult
    :: MonadInfer m 
    => Either [(Guard CF.Ann, Expr CF.Ann)] (Expr CF.Ann)
    -> m (Either [(Guard Ann, Expr Ann)] (Expr Ann), [Constraint])
annotateCaseResult (Right x') = do 
    (x, c) <- annotateExpr x'
    pure (Right x, c)
annotateCaseResult (Left guards') = do
    (guards, cs) <- flip mapAndUnzipM guards' $ \(g', x') -> do
        (g, c1) <- annotateExpr g'
        (x, c2) <- annotateExpr x'
        pure ((g, x), c1 <> c2)
    pure (Left guards, mconcat cs)
  

annotateBinder 
    :: MonadInfer m 
    => Go.Type
    -> Binder CF.Ann
    -> m (Binder Ann, TypeEnv, [Constraint])
annotateBinder t = \case
    NullBinder a -> do
        (ann, c) <- mkAnn a t
        pure (CF.NullBinder ann, mempty, c)

    VarBinder a ident -> do
        (ann, c) <- mkAnn a t
        let tenv = [(localVar ident, Forall [] t)]
        pure (CF.VarBinder ann ident, tenv, c)

    NamedBinder a ident binder' -> do
        let tenv1 = [(localVar ident, Forall [] t)]
        (ann, c1) <- mkAnn a t
        (binder, tenv2, c2) <- annotateBinder t binder'
        pure (NamedBinder ann ident binder, tenv2 <> tenv1, c1 <> c2)

    ConstructorBinder a tn cn binders' -> do
        (_, env, _) <- R.ask
        fields <- maybe undefined pure $
            Go.lookupConstructorFields tn (PS.disqualify cn) env
        let nt = Go.NamedType (Go.requalify (Go.TypeName <$> tn))
        (binders, tenvs, cs) <- unzip3 <$> 
            zipWithM annotateBinder (map snd fields) binders'
        (ann, c) <- mkAnn a nt
        pure (ConstructorBinder ann tn cn binders, mconcat tenvs, mconcat cs <> c)

    LiteralBinder a (NumericLiteral (Left integer)) -> do
        (ann, c1) <- mkAnn a t 
        let c2 = [(t, IntType)]
        pure (LiteralBinder ann (NumericLiteral (Left integer)), mempty, c1 <> c2)

    LiteralBinder a (NumericLiteral (Right double)) -> do
        (ann, c1) <- mkAnn a t 
        let c2 = [(t, Float64Type)]
        pure (LiteralBinder ann (NumericLiteral (Right double)), mempty, c1 <> c2)

    LiteralBinder a (StringLiteral psString) -> do
        (ann, c1) <- mkAnn a t 
        let c2 = [(t, StringType)]
        pure (LiteralBinder ann (StringLiteral psString), mempty, c1 <> c2)

    LiteralBinder a (CharLiteral char) -> do
        (ann, c1) <- mkAnn a t 
        let c2 = [(t, RuneType)]
        pure (LiteralBinder ann (CharLiteral char), mempty, c1 <> c2)

    LiteralBinder a (BooleanLiteral bool) -> do
        (ann, c1) <- mkAnn a t 
        let c2 = [(t, BoolType)]
        pure (LiteralBinder ann (BooleanLiteral bool), mempty, c1 <> c2)

    LiteralBinder _ (ArrayLiteral _) -> undefined
    LiteralBinder _ (ObjectLiteral _) -> undefined


-- ALGORITHM W STUFF


-- | Generalize a 'Type' to a 'Scheme' by universally quantifying over all the
-- type variables contained in it, except those already free in the environment.
generalize :: TypeEnv -> Type -> Scheme
generalize tenv t = Forall qs t
    where qs = S.difference (freeTVars t) (freeTVars tenv)


-- | Bind all quantified variables to fresh variables.
instantiate :: MonadSupply m => Scheme -> m Type
instantiate (Forall qs t) = do
    s <- substituteAllWithFresh qs
    pure (applySubst s t)


-- | For each type variable, add a substitution for a fresh type variable.
substituteAllWithFresh :: MonadSupply m => Set TVar -> m Subst
substituteAllWithFresh = fmap Subst . sequenceA . M.fromSet (const freshTVar) 


solve :: forall m. MonadError TypeError m => [Constraint] -> m Subst
solve cs = solver (mempty, cs)
  where
    solver :: (Subst, [Constraint]) -> m Subst
    solver (s, [])        = pure s
    solver (s1, c : rest) = do
        s2 <- unify c
        solver (s2 <> s1, applySubst s2 <$> rest)


-- | The unification of two 'Type's is the most general substitution that can
-- be applied to both of them in order to yield the same result.
--
-- For example, try following through the case of @unify (a, Int)@, which here 
-- would be more like @unify (TVar 0, IntType)@. The most general type that 
-- satisfies both is @Int@, so we need to return a substitution that will 
-- convert all occurrences of @a@ to @Int@.
unify :: MonadError TypeError m => (Type, Type) -> m Subst
unify = \case
    -- If the 'TVar' is frozen then don't bind it. 
    -- It will be asserted to the correct type later.
    (TVar v, x) | isFrozen v -> pure mempty 
                | otherwise  -> v `bindVariableTo` x
    (x, TVar v) | isFrozen v -> pure mempty
                | otherwise  -> v `bindVariableTo` x

    (FuncType a b, FuncType x y) -> do
        s1 <- unify (a, x)
        s2 <- unify (applySubst s1 (b, y))
        pure (s2 <> s1) 

    (MapType a b, MapType x y) -> do
        s1 <- unify (a, x)
        s2 <- unify (b, y)
        pure (s2 <> s1) 

    (SliceType a, SliceType b) -> unify (a, b)

    (PointerType x, y) -> unify (x, y)
    (x, PointerType y) -> unify (x, y)

    -- TODO: The others
    
    (a, b) | a == b    -> pure mempty
           | otherwise -> throwError (CannotUnify a b)


-- | Create a substitution that binds a type variable to the given type.
bindVariableTo :: MonadError TypeError m => TVar -> Type -> m Subst
bindVariableTo v t | TVar v == t    = pure mempty
                   | v `occursIn` t = throwError (InfiniteType v t)
                   | otherwise      = pure [(v, t)]


-- | Occurs check. 'False' here means we have an infinite type.
occursIn :: FreeTVars a => TVar -> a -> Bool
occursIn v t = v `S.member` freeTVars t


-- APPLYING SUBSTITUTIONS


class Substitutable a where
    applySubst :: Subst -> a -> a


-- These first few instances are just recursing to the 'Scheme' and 'Type' 
-- instances. They're boilerplate.

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
    applySubst s (a, b) = (applySubst s a, applySubst s b)

instance Substitutable a => Substitutable (Bind a) where
    applySubst s = fmap (applySubst s)

instance Substitutable a => Substitutable (Expr a) where
    -- NOTE: I don't think we need to use 'everywhereOnValues' here?
    applySubst s = fmap (applySubst s)

instance Substitutable Ann where
    applySubst s ann = ann { annType = applySubst s (annType ann) }

instance Substitutable Subst where
    applySubst s (Subst target) = Subst (applySubst s <$> target)

instance Substitutable TypeEnv where
    applySubst s (TypeEnv tenv) = TypeEnv (applySubst s <$> tenv)


-- Begin important instances:

instance Substitutable Scheme where
    applySubst (Subst s) (Forall qs t) = Forall qs (applySubst s' t)
        where qs' = M.fromSet (const ()) qs
              s'  = Subst (s `M.difference` qs')


instance Substitutable Type where
    applySubst s t = case t of
        -- Drill down...
        FuncType x y  -> FuncType (applySubst s x) (applySubst s y)
        SliceType v   -> SliceType (applySubst s v)
        MapType k v   -> MapType (applySubst s k) (applySubst s v)
        StructType fs -> StructType (fmap (applySubst s) <$> fs)
        PointerType x -> PointerType (applySubst s x)
        NilType x     -> NilType (applySubst s x)

        -- Yep
        TVar tv | isFrozen tv -> t
                | otherwise -> M.findWithDefault t tv (unSubst s)

        -- Nope
        IntType     -> t
        Float64Type -> t
        StringType  -> t
        RuneType    -> t
        BoolType    -> t
        NamedType{} -> t


-- EXTRACTING FREE TYPE VARIABLES


class FreeTVars a where
    freeTVars :: a -> Set TVar


instance FreeTVars TypeEnv where
    freeTVars (TypeEnv tenv) = S.unions (freeTVars <$> M.elems tenv)


instance FreeTVars Scheme where
    freeTVars (Forall qs t) = freeTVars t `S.difference` qs


instance FreeTVars Type where
    freeTVars t = case t of
        -- Drill down...
        FuncType a b  -> freeTVars a <> freeTVars b
        SliceType v   -> freeTVars v
        MapType k v   -> freeTVars k <> freeTVars v
        StructType fs -> foldMap (freeTVars . snd) fs
        PointerType x -> freeTVars x
        NilType x     -> freeTVars x

        -- Yep
        TVar tv       | isFrozen tv -> mempty 
                      | otherwise -> [tv]

        -- Nope
        IntType       -> mempty
        Float64Type   -> mempty
        StringType    -> mempty
        RuneType      -> mempty
        BoolType      -> mempty
        NamedType{}   -> mempty


-- UTIL


extractAnnType :: CF.Expr Ann -> Type
extractAnnType = annType . CF.extractAnn


throwUnimplemented :: MonadError TypeError m => Expr a -> m b
throwUnimplemented x = throwError (Unimplemented (() <$ x))


-- Useful references:
--
-- https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter7/poly/src/Infer.hs
-- https://github.com/quchen/articles/blob/master/hindley-milner/src/HindleyMilner.hs