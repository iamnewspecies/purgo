module Prelude where

identity :: forall a. a -> a
identity a = a

const :: forall a b. a -> b -> a
const a _ = a

flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f b a = f a b

apply :: forall a b. (a -> b) -> a -> b
apply f a = f a
