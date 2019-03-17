{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Language.Go.Names
    ( Ident
    , Var
    , Field
    , Visible(..)
    , PackageName(..)
    , Qualified(..)
    , TypeName(..)
    , FieldName(..)
    , runPackageName
    , requalify
    , unqualifyFor
    )
where

import Prelude

import Data.Text (Text)
import Language.PureScript.PSString (PSString)

import qualified Data.Text as T
import qualified Language.PureScript.Label as PS (Label)
import qualified Language.PureScript.Names as PS


type Ident = Visible PS.Ident

type Var = Qualified Ident

type Field = Visible FieldName


data Visible a
    = Public a    -- ^ Must begin with an __upper case__ letter
    | Private a   -- ^ Must begin with a __lower case__ letter
    | Local a     -- ^ Doesn't matter what it begins with
    deriving (Functor, Show, Eq, Ord)
    -- NOTE: A function argument /can/ be bound to an upper case identifier (lol).


-- | The /name/ of a Go package (not the import path).
--
-- > PackageName "fmt"
newtype PackageName = PackageName { unPackageName :: PS.ModuleName }
    deriving newtype (Show, Eq, Ord)


runPackageName :: PackageName -> Text
runPackageName (PackageName (PS.ModuleName pns)) = 
    T.intercalate "_" (PS.runProperName <$> pns)


-- | A qualified name, i.e. a name that /could/ have a package qualifier.
--
-- > "fmt.Println" == Qualified (Just "fmt") "Println"
-- > "foo" == Qualified Nothing "foo"
data Qualified a = Qualified (Maybe PackageName) a
    deriving (Show, Eq, Ord, Functor)


-- | Convert a PureScript qualified name to a Go qualified name.
requalify :: PS.Qualified a -> Qualified a
requalify (PS.Qualified m a) = Qualified (PackageName <$> m) a


unqualifyFor :: PackageName -> Qualified a -> Qualified a
unqualifyFor pn (Qualified (Just pn') a) 
    | pn == pn' = Qualified Nothing a
unqualifyFor _ q = q


-- | The name of Go types.
-- 
-- @
-- type TypeName struct {
--     foo int
-- }
-- @
-- 
-- This way we don't lose information about a types origin.
data TypeName
    = TypeName (PS.ProperName 'PS.TypeName)
    | ClassName (PS.ProperName 'PS.ClassName)
    deriving (Show, Eq, Ord)


-- | Name of a struct field.
--
-- What's the point? We compile a few different things into structs, and I want
-- to preserve as much information as possible.
data FieldName
    = IdentField PS.Ident 
    -- ^ Plain identifier

    | StringField PSString  
    -- ^ Key of an @ObjectLiteral@
    
    | ConstructorField (PS.ProperName 'PS.ConstructorName)
    -- ^ Individual data constructor

    | LabelField PS.Label
    -- ^ Label from a Row type
    
    deriving (Show, Eq)
