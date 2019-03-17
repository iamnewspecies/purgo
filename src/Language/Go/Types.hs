{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Go.Types 
    ( Type(..) 
    , pattern ObjectType
    , pattern EmptyInterfaceType
    , TVar
    , freshTVar
    , isFrozen
    , isTVar
    , freezeTVars
    , typeFromPS
    , UnsupportedType(..)
    , subTypes
    , unFunc
    , typeDeclsForModules
    )
where

import Prelude

import Control.Monad (foldM)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Supply.Class (MonadSupply, fresh)
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)

import Cheapskate.Plate (rewriteOf)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Language.PureScript.AST as PS
import qualified Language.PureScript.Constants as PS
import qualified Language.PureScript.Label as PS (Label)
import qualified Language.PureScript.Names as PS
import qualified Language.PureScript.Types as PS

import qualified Language.Go.Names as Go


-- | Go types.
--
-- Not exhaustive - only covering the types we expect to work with here. FFI
-- code may use the others.
data Type
    = FuncType Type Type   -- ^ @func(int) int@
    | IntType              -- ^ @int@
    | Float64Type          -- ^ @float64@
    | StringType           -- ^ @string@
    | RuneType             -- ^ @rune@
    | BoolType             -- ^ @bool@
    | SliceType Type       -- ^ @[]int@
    | MapType Type Type    -- ^ @map[string]int@
    | PointerType Type     -- ^ @*string@
    | NilType Type         -- ^ @nil@

    | StructType [(Go.Field, Type)] 
    -- ^ @struct { foo int; bar string }@

    | NamedType (Go.Qualified Go.TypeName) 
    -- ^ Go's version of a type alias

    | TVar !TVar
    -- ^ This constructor exists for the sake of inference. Without the
    -- integer variable, it is @interface{}@
    deriving (Show, Eq)


-- | @interface{}@
pattern EmptyInterfaceType :: Type
pattern EmptyInterfaceType <- TVar _


-- | @map[string]interface{}@
pattern ObjectType :: Type
pattern ObjectType <- MapType StringType EmptyInterfaceType


-- | A type variable, used for inference.
--
-- We need to track whether a 'TVar' is frozen because once we decide on a type
-- during inference (in this case @interface{}@) it cannot be changed. Thus, a
-- "frozen" type variable will not be bound during unification. If there's a 
-- better way to handle this that doesn't escape the inference logic I'd love
-- to here it.
data TVar = TV { tvFrozen :: !Bool, tvInteger :: !Integer }
    deriving (Show)


instance Eq TVar where (==) = (==) `on` tvInteger
instance Ord TVar where compare = compare `on` tvInteger


-- | Spit out a fresh type variable.
freshTVar :: MonadSupply m => m Type
freshTVar = TVar . TV False <$> fresh


isTVar :: Type -> Bool
isTVar (TVar _) = True
isTVar _        = False


isFrozen :: TVar -> Bool
isFrozen = tvFrozen


freezeTVars :: Type -> Type
freezeTVars = rewriteOf subTypes $ \case
    -- NOTE: Need to match explicitly here so that 'rewriteOf' doesn't 
    -- get stuck in a loop.
    TVar (TV False i) -> Just (TVar (TV True i))
    _ -> Nothing


-- | Try and convert a PureScript type to a Go type.
--
-- The error is the type that couldn't be converted.
typeFromPS 
    :: (MonadSupply m, MonadError (UnsupportedType a) m) 
    => PS.Type a 
    -> m Type
typeFromPS = \case
    PS.ForAll _ _ t _ ->
        typeFromPS t

    PS.KindedType _ t _ ->
        typeFromPS t

    PS.ParensInType _ t ->
        -- Need this because we sometimes deal with sugared types
        -- E.g. 'typeDeclsForModules' below
        typeFromPS t

    FunctionApp x y ->
        FuncType <$> typeFromPS x <*> typeFromPS y

    PrimType "Boolean" ->
        pure BoolType

    PrimType "Int" ->
        pure IntType

    PrimType "Number" ->
        pure Float64Type

    PrimType "String" ->
        pure StringType

    PrimType "Char" ->
        pure RuneType

    PS.TypeApp _ (PrimType "Array") itemType ->
        SliceType <$> typeFromPS itemType

    PS.TypeApp _ t _ ->
        -- Drill down to the left-most type because we don't have 
        -- higher-kindednesss in Go
        typeFromPS t

    PS.TypeVar{} ->
        TVar . TV False <$> fresh

    PS.Skolem{} ->
        TVar . TV False <$> fresh
    
    PS.ConstrainedType _ c t ->
        let nt = NamedType (Go.requalify (Go.ClassName <$> PS.constraintClass c))
         in FuncType nt <$> typeFromPS t

    row@PS.RCons{} -> 
        StructType <$>
            traverse (sequenceA . bimap (Go.Private . Go.LabelField) typeFromPS) (unRow row)
            --                           ^^^^^^^^^^
            --                      How should we handle this!?

    PS.TypeConstructor _ (PS.Qualified mn tn) -> 
         pure (NamedType (Go.Qualified (Go.PackageName <$> mn) (Go.TypeName tn)))

    other -> 
        throwError (UnsupportedType other)


newtype UnsupportedType a = UnsupportedType (PS.Type a)
    deriving stock (Show)


-- | Traversal over the immediate sub-types of a type.
subTypes :: Applicative f => (Type -> f Type) -> Type -> f Type
subTypes f = \case
    IntType            -> pure IntType
    Float64Type        -> pure Float64Type
    StringType         -> pure StringType
    RuneType           -> pure RuneType
    BoolType           -> pure BoolType
    NamedType typeName -> pure (NamedType typeName)
    TVar tv            -> pure (TVar tv)

    FuncType x y       -> FuncType <$> f x <*> f y
    SliceType t        -> SliceType <$> f t
    MapType k v        -> MapType <$> f k <*> f v
    StructType fs      -> StructType <$> traverse (traverse f) fs
    PointerType t      -> PointerType <$> f t
    NilType t          -> NilType <$> f t


-- | Flatten out @func@ arguments.
unFunc :: Type -> [Type]
unFunc (FuncType x y) = unFunc x <> unFunc y
unFunc t              = [t]
{-# INLINE unFunc #-}


-- | Flatten out a Row type.
unRow :: PS.Type a -> [(PS.Label, PS.Type a)]
unRow (PS.RCons _ label t (PS.REmpty _)) = [(label, t)]
unRow (PS.RCons _ label t rest)          = (label, t) : unRow rest
unRow _                                  = [] -- error?


-- | Pull type declarations out of modules
--
-- This is a rather unfortunate hack due to the fact that a) @CoreFn@ doesn't
-- give us all the type information we need to form Go type declarations and b)
-- @ExternsFiles@ only tell us about exported things.
--
-- Note that the (successful) result type permits the following: 
--
-- > uncurry Go.TypeDecl <$> Map.assocs result
--
typeDeclsForModules 
    :: (MonadSupply m, MonadError (UnsupportedType PS.SourceAnn) m)
    => [PS.Module]
    -> m (M.Map (PS.Qualified Go.TypeName) Type)
typeDeclsForModules = foldM extractType M.empty 


extractType
    :: forall m . (MonadSupply m, MonadError (UnsupportedType PS.SourceAnn) m)
    => M.Map (PS.Qualified Go.TypeName) Type
    -> PS.Module
    -> m (M.Map (PS.Qualified Go.TypeName) Type)
extractType accum (PS.Module _ _ mn decls exports) = do
    types <- catMaybes <$> traverse typeFromDataDeclaration decls
    pure (M.fromList types <> accum)
  where
    typeFromDataDeclaration :: PS.Declaration -> m (Maybe (PS.Qualified Go.TypeName, Type))
    typeFromDataDeclaration = \case
        PS.DataDeclaration _ _ tn _ ctors -> do
            ctorFields <- traverse (uncurry constructorField) ctors
            pure $ Just (PS.Qualified (Just mn) (Go.TypeName tn), StructType ctorFields)

        _ -> pure Nothing

    constructorField  
      :: PS.ProperName 'PS.ConstructorName -> [PS.SourceType] -> m (Go.Field, Type)
    constructorField cn values = do
        fields <- traverse (uncurry mkField) (zip [0..] values)
        pure (visible (Go.ConstructorField cn), PointerType (StructType fields))
      where
        visible :: a -> Go.Visible a
        visible = if cn `S.member` exportedConstructors then Go.Public else Go.Private

        mkField :: Int -> PS.SourceType -> m (Go.Field, Type)
        mkField i st = do
            let fieldName = visible (Go.IdentField (valueIdent i))
            fieldType <- typeFromPS st
            pure (fieldName, fieldType)
   
    exportedConstructors :: S.Set (PS.ProperName 'PS.ConstructorName)
    exportedConstructors = S.fromList . mconcat $
        [ ctors | PS.TypeRef _ _ (Just ctors) <- fromMaybe [] exports ]
    
    -- Ew. This is strongly coupled to an (arbitrary) compiler implementation
    -- detail. But I don't think there's any other way. At this point we don't
    -- have @Constructor@ idents.
    valueIdent :: Int -> PS.Ident
    valueIdent i = PS.Ident $ T.pack ("value" <> show i)


-- PATTERNS


pattern PrimType :: Text -> PS.Type a
pattern PrimType t <- PS.TypeConstructor _ (Prim t)


pattern Prim :: Text -> PS.Qualified (PS.ProperName a)
pattern Prim t <- PS.Qualified (Just PS.Prim) (PS.ProperName t)


pattern FunctionApp :: PS.Type a -> PS.Type a -> PS.Type a
pattern FunctionApp lhs rhs <-
    PS.TypeApp _ (PS.TypeApp _ (PrimType "Function") lhs) rhs
