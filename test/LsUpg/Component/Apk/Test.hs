{-# LANGUAGE OverloadedStrings #-}

module LsUpg.Component.Apk.Test (tests) where

-- https://hackage.haskell.org/package/bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- (lsupg)
import qualified LsUpg.Component as Component
import qualified LsUpg.Component.Apk as Apk

------------------------------------------------------------------------------


testParseItemsNone :: TestTree
testParseItemsNone = testCase "none" $
    ([], []) @=? Apk.parseItems output
  where
    output :: ByteString
    output = BSL8.unlines
      [ "OK: 6 MiB in 14 packages"
      ]

------------------------------------------------------------------------------

upgradeItems :: [Component.Item]
upgradeItems =
    [ Component.Item
        { Component.componentName    = Apk.name
        , Component.itemName         = "musl"
        , Component.installedVersion = Just "1.2.2-r0"
        , Component.availableVersion = Just "1.2.2-r1"
        }
    , Component.Item
        { Component.componentName    = Apk.name
        , Component.itemName         = "musl-utils"
        , Component.installedVersion = Just "1.2.2-r0"
        , Component.availableVersion = Just "1.2.2-r1"
        }
    ]

testParseItemsUpgrade :: TestTree
testParseItemsUpgrade = testCase "upgrade" $
    ([], upgradeItems) @=? Apk.parseItems output
  where
    output :: ByteString
    output = BSL8.unlines
      [ "(1/2) Upgrading musl (1.2.2-r0 -> 1.2.2-r1)"
      , "(2/2) Upgrading musl-utils (1.2.2-r0 -> 1.2.2-r1)"
      , "OK: 6 MiB in 14 packages"
      ]

------------------------------------------------------------------------------

testParseItemsError :: TestTree
testParseItemsError = testCase "error" $
    ([err], upgradeItems) @=? Apk.parseItems output
  where
    err :: String
    err = "error parsing apk line: (2/3) Upgrading error invalid line"

    output :: ByteString
    output = BSL8.unlines
      [ "(1/3) Upgrading musl (1.2.2-r0 -> 1.2.2-r1)"
      , "(2/3) Upgrading error invalid line"
      , "(3/3) Upgrading musl-utils (1.2.2-r0 -> 1.2.2-r1)"
      , "OK: 6 MiB in 14 packages"
      ]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "LsUpg.Component.Apk"
    [ testGroup "parseItems"
        [ testParseItemsNone
        , testParseItemsUpgrade
        , testParseItemsError
        ]
    ]
