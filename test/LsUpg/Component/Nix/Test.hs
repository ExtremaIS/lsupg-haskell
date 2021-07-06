{-# LANGUAGE OverloadedStrings #-}

module LsUpg.Component.Nix.Test (tests) where

-- https://hackage.haskell.org/package/bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- https://hackage.haskell.org/package/text
import Data.Text (Text)

-- https://hackage.haskell.org/package/unordered-containers
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

-- (lsupg)
import qualified LsUpg.Component as Component
import qualified LsUpg.Component.Nix as Nix

------------------------------------------------------------------------------

testParsePackagesNone :: TestTree
testParsePackagesNone = testCase "none" $
    ([], HashMap.empty) @=? Nix.parsePackages ""

------------------------------------------------------------------------------

testParsePackagesValid :: TestTree
testParsePackagesValid = testCase "valid" $
    ([], packages) @=? Nix.parsePackages output
  where
    output :: ByteString
    output = BSL8.unlines
      [ "0ad-0.0.24b"
      , "nix-2.3.11"
      , "python3-3.8.5"
      ]

    packages :: HashMap Text Text
    packages = HashMap.fromList
      [ ("0ad", "0.0.24b")
      , ("nix", "2.3.11")
      , ("python3", "3.8.5")
      ]

------------------------------------------------------------------------------

testParsePackagesInvalid :: TestTree
testParsePackagesInvalid = testCase "invalid" $
    (errs, packages) @=? Nix.parsePackages output
  where
    output :: ByteString
    output = BSL8.unlines
      [ "no-version"
      , "noversion"
      , "nix-2.3.11"
      , ""
      ]

    errs :: [String]
    errs =
      [ "error parsing nix line: no-version"
      , "error parsing nix line: noversion"
      , "error parsing nix line: "
      ]

    packages :: HashMap Text Text
    packages = HashMap.singleton "nix" "2.3.11"

------------------------------------------------------------------------------

testResolveItems :: TestTree
testResolveItems = testCase "resolveItems" $
    items @=? Nix.resolveItems currentPackages targetPackages
  where
    currentPackages, targetPackages :: HashMap Text Text
    currentPackages = HashMap.fromList
      [ ("current-only", "0")
      , ("nix", "2.3.11")
      , ("python3", "3.8.5")
      ]
    targetPackages = HashMap.fromList
      [ ("nix", "2.3.12")
      , ("python3", "3.8.9")
      , ("target-only", "0")
      ]

    items :: [Component.Item]
    items =
      [ Component.Item
          { Component.componentName    = Nix.name
          , Component.itemName         = "current-only"
          , Component.installedVersion = Just "0"
          , Component.availableVersion = Nothing
          }
      , Component.Item
          { Component.componentName    = Nix.name
          , Component.itemName         = "nix"
          , Component.installedVersion = Just "2.3.11"
          , Component.availableVersion = Just "2.3.12"
          }
      , Component.Item
          { Component.componentName    = Nix.name
          , Component.itemName         = "python3"
          , Component.installedVersion = Just "3.8.5"
          , Component.availableVersion = Just "3.8.9"
          }
      , Component.Item
          { Component.componentName    = Nix.name
          , Component.itemName         = "target-only"
          , Component.installedVersion = Nothing
          , Component.availableVersion = Just "0"
          }
      ]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "LsUpg.Component.Nix"
    [ testGroup "parsePackages"
        [ testParsePackagesNone
        , testParsePackagesValid
        , testParsePackagesInvalid
        ]
    , testResolveItems
    ]
