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

testParseItemsNone :: TestTree
testParseItemsNone = testCase "none" $
    ([], []) @=? Pacman.parseItems output
  where
    output :: ByteString
    output = ""

------------------------------------------------------------------------------

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

testParseItemsUpgrade :: TestTree
testParseItemsUpgrade = testCase "upgrade" $
    ([], upgradeItems) @=? Pacman.parseItems output
  where
    output :: ByteString
    output = BSL8.unlines
      [ "pacman 5.2.2-3 -> 6.0.0-3"
      , "sqlite 3.35.5-1 -> 3.35.5-2"
      ]

------------------------------------------------------------------------------

testParseItemsError :: TestTree
testParseItemsError = testCase "error" $
    ([err], upgradeItems) @=? Pacman.parseItems output
  where
    err :: String
    err = "error parsing pacman line: error invalid line"

    output :: ByteString
    output = BSL8.unlines
      [ "pacman 5.2.2-3 -> 6.0.0-3"
      , "error invalid line"
      , "sqlite 3.35.5-1 -> 3.35.5-2"
      ]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "LsUpg.Component.Pacman"
    [ testGroup "parseItems"
        [ testParseItemsNone
        , testParseItemsUpgrade
        , testParseItemsError
        ]
    ]
