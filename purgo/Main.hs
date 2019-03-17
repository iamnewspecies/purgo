module Main (main) where

import Prelude

import qualified Command.Compile as Compile


main :: IO ()
main = do
    putStrLn "Running..."
    Compile.run ["example/Prelude.purs", "example/Data/Maybe.purs", "example/Example.purs"]
