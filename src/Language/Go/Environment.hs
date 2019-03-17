{-# LANGUAGE DataKinds #-}
module Language.Go.Environment 
    ( Environment(..)
    , emptyEnvironment
    , extendEnvironmentWithBinds
    , lookupConstructorFields
    , names
    , types
    ) 
where

import Prelude

import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Data.Map (Map)
import Lens.Micro.Platform (over)

import qualified Data.Map as M
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.Names as PS

import qualified Language.Go.Names as Go
import qualified Language.Go.Types as Go


-- | Type environment.
data Environment = Environment
    { envNames :: Map (PS.Qualified PS.Ident) Go.Type
    -- ^ Populated from (top-level) @var@, @const@, and @func@ declarations.
    , envTypes :: Map (PS.Qualified Go.TypeName) Go.Type
    -- ^ Populated from (top-level) @type@ declarations. These will mostly
    -- be struct-type definitions.
    }
    deriving (Show)


-- | An initial, empty 'Environment'.
emptyEnvironment :: Environment
emptyEnvironment = Environment 
    { envNames = M.empty
    , envTypes = M.empty 
    }


-- | Extend the 'Environment' with some (exported) Go annotated bindings.
extendEnvironmentWithBinds 
    :: PS.ModuleName
    -> [PS.Ident]
    -> [CF.Bind Go.Type]
    -> Environment 
    -> Environment
extendEnvironmentWithBinds mn exports = flip . foldl' $ \accum bind -> 
    case bind of
        CF.NonRec ann ident expr ->
            extendWithBind ann ident expr accum

        CF.Rec rec ->
            foldl' (flip (uncurry (uncurry extendWithBind))) accum rec
  where
    extendWithBind 
        :: Go.Type -> PS.Ident -> CF.Expr Go.Type -> Environment
        -> Environment
    extendWithBind t ident _ env 
        | ident `notElem` exports = env  -- no op
        | otherwise = over names (M.insert (PS.Qualified (Just mn) ident) t) env


-- | Lookup constructor fields in the current 'Environment'.
lookupConstructorFields
    :: PS.Qualified (PS.ProperName 'PS.TypeName)
    -> PS.ProperName 'PS.ConstructorName
    -> Environment
    -> Maybe [(Go.Field, Go.Type)]
lookupConstructorFields tn cn env = do
    Go.StructType ctors <- M.lookup (Go.TypeName <$> tn) (envTypes env)
    Go.PointerType (Go.StructType fields) <- 
        -- NOTE: Unfortunately we can't depend on the caller knowing the visibility 
        -- of the constructor. But constructors are only ever defined at the top 
        -- level, so we know they will be either 'Public' or 'Private'...
        lookup (Go.Public  (Go.ConstructorField cn)) ctors <|>
        lookup (Go.Private (Go.ConstructorField cn)) ctors 
    pure fields


-- LENSES


names 
    :: Functor f 
    => (Map (PS.Qualified PS.Ident) Go.Type -> f (Map (PS.Qualified PS.Ident) Go.Type))
    -> Environment
    -> f Environment
names f env = (\nms -> env { envNames = nms }) <$> f (envNames env)


types 
    :: Functor f 
    => (Map (PS.Qualified Go.TypeName) Go.Type -> f (Map (PS.Qualified Go.TypeName) Go.Type))
    -> Environment
    -> f Environment
types f env = (\ts -> env { envTypes = ts }) <$> f (envTypes env)
