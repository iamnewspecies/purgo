module Language.Go.Crash 
    ( internalError
    , impossible
    )
where

import Prelude

import GHC.Stack (HasCallStack)


internalError :: HasCallStack => String -> a
internalError why = error $ 
    why <> "\n\n" <>
    "Well this is awkward." <> "\n" <>
    "Please report to " <> issuesLink <> "\n"


impossible :: HasCallStack => a
impossible = error $
    rage <> "\n\n" <>
    "This wasn't supposed to happen." <> "\n" <>
    "Please report to " <> issuesLink <> "\n"


rage :: String
rage = "(╯°□°）╯︵┻━┻"


issuesLink :: String
issuesLink = "https://github.com/purescript-go/purgo/issues"
