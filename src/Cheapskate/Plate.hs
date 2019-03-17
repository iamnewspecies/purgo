module Cheapskate.Plate 
    ( rewriteOf
    , transformOf
    )
where

import Prelude

import Lens.Micro.Platform (ASetter, over)


rewriteOf :: ASetter a b a b -> (b -> Maybe a) -> a -> b
rewriteOf l f = go where go = transformOf l (\x -> maybe x go (f x))
{-# INLINE rewriteOf #-}


transformOf :: ASetter a b a b -> (b -> b) -> a -> b
transformOf l f = go where go = f . over l go
{-# INLINE transformOf #-}
