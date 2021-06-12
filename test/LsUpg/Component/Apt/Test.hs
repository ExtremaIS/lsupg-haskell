{-# LANGUAGE OverloadedStrings #-}

module LsUpg.Component.Apt.Test (tests) where

-- https://hackage.haskell.org/package/bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- (lsupg)
import qualified LsUpg.Component as Component
import qualified LsUpg.Component.Apt as Apt

------------------------------------------------------------------------------

noneOutput :: ByteString
noneOutput = BSL8.unlines
    [ "Reading package lists... Done"
    , "Building dependency tree       "
    , "Reading state information... Done"
    , "Calculating upgrade... Done"
    , "0 upgraded, 0 newly installed, 0 to remove and 0 not upgraded."
    ]

testNone :: TestTree
testNone = testCase "none" $
    ([], []) @=? Apt.parseItems noneOutput

------------------------------------------------------------------------------

upgradeOutput :: ByteString
upgradeOutput = BSL8.unlines
    [ "Reading package lists... Done"
    , "Building dependency tree       "
    , "Reading state information... Done"
    , "Calculating upgrade... Done"
    , "The following packages will be upgraded:"
    , "  liblz4-1"
    , "1 upgraded, 0 newly installed, 0 to remove and 0 not upgraded."
    , "Inst liblz4-1 [1.8.3-1] (1.8.3-1+deb10u1 Debian-Security:10/stable [amd64])"
    , "Conf liblz4-1 (1.8.3-1+deb10u1 Debian-Security:10/stable [amd64])"
    ]

upgradeItem :: Component.Item
upgradeItem = Component.Item
    { Component.componentName    = Apt.name
    , Component.itemName         = "liblz4-1"
    , Component.installedVersion = Just "1.8.3-1"
    , Component.availableVersion = Just "1.8.3-1+deb10u1"
    }

testUpgrade :: TestTree
testUpgrade = testCase "upgrade" $
    ([], [upgradeItem]) @=? Apt.parseItems upgradeOutput

------------------------------------------------------------------------------

newOutput :: ByteString
newOutput = BSL8.unlines
    [ "Reading package lists... Done"
    , "Building dependency tree       "
    , "Reading state information... Done"
    , "The following NEW packages will be installed:"
    , "  zip"
    , "0 upgraded, 1 newly installed, 0 to remove and 0 not upgraded."
    , "Inst zip (3.0-11+b1 Debian:10.9/stable [amd64])"
    , "Conf zip (3.0-11+b1 Debian:10.9/stable [amd64])"
    ]

newItem :: Component.Item
newItem = Component.Item
    { Component.componentName    = Apt.name
    , Component.itemName         = "zip"
    , Component.installedVersion = Nothing
    , Component.availableVersion = Just "3.0-11+b1"
    }

testNew :: TestTree
testNew = testCase "new" $
    ([], [newItem]) @=? Apt.parseItems newOutput

------------------------------------------------------------------------------

errorOutput :: ByteString
errorOutput = BSL8.unlines
    [ "Reading package lists... Done"
    , "Building dependency tree       "
    , "Reading state information... Done"
    , "Calculating upgrade... Done"
    , "The following NEW packages will be installed:"
    , "  zip"
    , "The following packages will be upgraded:"
    , "  liblz4-1"
    , "1 upgraded, 1 newly installed, 0 to remove and 0 not upgraded."
    , "Inst zip (3.0-11+b1 Debian:10.9/stable [amd64])"
    , "Inst error invalid line"
    , "Inst liblz4-1 [1.8.3-1] (1.8.3-1+deb10u1 Debian-Security:10/stable [amd64])"
    , "Conf zip (3.0-11+b1 Debian:10.9/stable [amd64])"
    , "Conf liblz4-1 (1.8.3-1+deb10u1 Debian-Security:10/stable [amd64])"
    ]

testError :: TestTree
testError = testCase "error" $
    ([err], [newItem, upgradeItem]) @=? Apt.parseItems errorOutput
  where
    err :: String
    err = "error parsing apt line: Inst error invalid line"

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "LsUpg.Component.Apt"
    [ testNone
    , testUpgrade
    , testNew
    , testError
    ]
