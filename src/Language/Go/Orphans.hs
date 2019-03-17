{-# OPTIONS -fno-warn-orphans #-}
module Language.Go.Orphans () where

import Prelude ()

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Supply.Class (MonadSupply)


instance MonadSupply m => MonadSupply (ReaderT r m)
instance MonadSupply m => MonadSupply (ExceptT e m)
