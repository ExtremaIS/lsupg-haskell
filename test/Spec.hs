module Main (main) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (defaultMain, testGroup)

-- (lsupg:test)
import qualified LsUpg.Component.Apt.Test

------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "main"
    [ LsUpg.Component.Apt.Test.tests
    ]
