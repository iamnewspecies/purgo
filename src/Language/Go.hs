module Language.Go 
    ( module Go
    , version
    ) 
where

import Prelude ()

import Data.Version (Version)

import Language.Go.Ann as Go
import Language.Go.AST as Go
import Language.Go.Compile as Go
import Language.Go.Constants as Go
import Language.Go.Environment as Go
import Language.Go.File as Go
import Language.Go.Names as Go
import Language.Go.Printer as Go
import Language.Go.Tidy as Go
import Language.Go.Types as Go

import qualified Paths_purgo as Paths


version :: Version
version = Paths.version
