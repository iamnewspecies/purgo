module Data.Maybe where

import Prelude

data Maybe a = Just a | Nothing

maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
maybe b _ Nothing = b
maybe _ f (Just a) = f a

fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe a = maybe a identity
