module Example where

import Prelude
import Data.Maybe

-- FIXME
--apply :: forall a b. (a -> b) -> a -> b
--apply f a = f a

two :: Int
two = apply idInt 2

idInt :: Int -> Int
idInt int = int

five :: Int
five = flip maybe identity 5 (Just 5)
--five = maybe 5 identity (Just 5)

--
--noPanic :: Maybe Int -> Int
--noPanic (Just i) = i
--noPanic _ = 42
--
--slice :: Array Int
--slice = [1, 2, 3, 4]
--
--matchSlice :: Array Int -> Int
--matchSlice [ 42 ] = 0
--matchSlice _ = 1


--matchInt :: Int -> Int
--matchInt 42 = 43
--matchInt _ = 0


--wierd :: Maybe (Maybe Int) -> Maybe Int
--wierd (Just x@(Just 2)) = x
--wierd _ = Nothing


--one :: Int
--one = 1
--
--
--data Unit = Unit
--
--data List a = Cons a (List a) | Nil
--
--
--myList :: List Int
--myList = Cons 1 (Cons 2 (Nil))
--
--
--alwaysFoo :: forall a. a -> { foo :: String, bar :: Number }
--alwaysFoo _ = { foo: "foo", bar: 42.0 }


--foo :: forall a. Example a
--foo = Foo 42
--
--
--one :: Int
--one = 1
--
--
--two :: Int
--two = 2


--rec :: { foo :: Int, bar :: String }
--rec = { foo: 42, bar: "bar" }


--identity :: forall a. a -> a
--identity a = a
--

--const :: forall a b. a -> b -> a
--const a _ = a


---- Recursive binds
--
--
--f :: forall a. a -> a
--f x = g x
--
--
--g :: forall a. a -> a
--g x = f x
--
--
---- TOOD
--
----data F = A | B
