module Main (main) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (defaultMain, testGroup)

-- (lsupg:test)
import qualified LsUpg.Component.Apk.Test
import qualified LsUpg.Component.Apt.Test
import qualified LsUpg.Component.Nix.Test
import qualified LsUpg.Component.Pacman.Test

------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "main"
    [ LsUpg.Component.Apk.Test.tests
    , LsUpg.Component.Apt.Test.tests
    , LsUpg.Component.Nix.Test.tests
    , LsUpg.Component.Pacman.Test.tests
    ]
