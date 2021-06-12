{-# LANGUAGE OverloadedStrings #-}

module LsUpg.Component.Pacman.Test (tests) where

-- https://hackage.haskell.org/package/bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- (lsupg)
import qualified LsUpg.Component as Component
import qualified LsUpg.Component.Pacman as Pacman

------------------------------------------------------------------------------

noneOutput :: ByteString
noneOutput = ""

testNone :: TestTree
testNone = testCase "none" $
    ([], []) @=? Pacman.parseItems noneOutput

------------------------------------------------------------------------------

upgradeOutput :: ByteString
upgradeOutput = BSL8.unlines
    [ "pacman 5.2.2-3 -> 6.0.0-3"
    , "sqlite 3.35.5-1 -> 3.35.5-2"
    ]

upgradeItems :: [Component.Item]
upgradeItems =
    [ Component.Item
        { Component.componentName    = Pacman.name
        , Component.itemName         = "pacman"
        , Component.installedVersion = Just "5.2.2-3"
        , Component.availableVersion = Just "6.0.0-3"
        }
    , Component.Item
        { Component.componentName    = Pacman.name
        , Component.itemName         = "sqlite"
        , Component.installedVersion = Just "3.35.5-1"
        , Component.availableVersion = Just "3.35.5-2"
        }
    ]

testUpgrade :: TestTree
testUpgrade = testCase "upgrade" $
    ([], upgradeItems) @=? Pacman.parseItems upgradeOutput

------------------------------------------------------------------------------

errorOutput :: ByteString
errorOutput = BSL8.unlines
    [ "pacman 5.2.2-3 -> 6.0.0-3"
    , "error invalid line"
    , "sqlite 3.35.5-1 -> 3.35.5-2"
    ]

testError :: TestTree
testError = testCase "error" $
    ([err], upgradeItems) @=? Pacman.parseItems errorOutput
  where
    err :: String
    err = "error parsing pacman line: error invalid line"

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "LsUpg.Component.Pacman"
    [ testNone
    , testUpgrade
    , testError
    ]
