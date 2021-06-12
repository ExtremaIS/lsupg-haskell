module Main (main) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (defaultMain, testGroup)

-- (lsupg:test)
import qualified LsUpg.Component.Apt.Test
import qualified LsUpg.Component.Nix.Test

------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "main"
    [ LsUpg.Component.Apt.Test.tests
    , LsUpg.Component.Nix.Test.tests
    ]
