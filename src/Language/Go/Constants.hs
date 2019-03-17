{-# OPTIONS -fno-warn-missing-export-lists #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Go.Constants where

import Prelude ()

import Data.String (IsString)


defaultFileName :: IsString s => s
defaultFileName = "module.go"


publicPrefix :: IsString s => s
publicPrefix = "Public__"


privatePrefix :: IsString s => s
privatePrefix = "private__"


typeSuffix :: IsString s => s
typeSuffix = "__Type"


classSuffix :: IsString s => s
classSuffix = "__Class"
