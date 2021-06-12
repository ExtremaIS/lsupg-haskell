{-# LANGUAGE OverloadedStrings #-}

module LsUpg.Component.Dnf.Test (tests) where

-- https://hackage.haskell.org/package/bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- (lsupg)
import qualified LsUpg.Component as Component
import qualified LsUpg.Component.Dnf as Dnf

------------------------------------------------------------------------------

noneOutput :: ByteString
noneOutput = BSL8.unlines
    [ -- TODO
    ]

testNone :: TestTree
testNone = testCase "none" $
    ([], []) @=? Dnf.parseItems noneOutput

------------------------------------------------------------------------------

upgradeOutput :: ByteString
upgradeOutput = BSL8.unlines
    [ "Fedora 34 openh264 (From Cisco) - x86_64                1.7 kB/s | 2.5 kB     00:01    "
    , "Fedora Modular 34 - x86_64                              2.6 MB/s | 4.9 MB     00:01    "
    , "Fedora Modular 34 - x86_64 - Updates                    2.3 MB/s | 4.2 MB     00:01    "
    , "Fedora 34 - x86_64 - Updates                            4.1 MB/s |  15 MB     00:03    "
    , "Fedora 34 - x86_64                                      6.6 MB/s |  74 MB     00:11    "
    , "Last metadata expiration check: 0:00:01 ago on Sat Jun 12 10:07:05 2021."
    , ""
    , "coreutils.x86_64                                 8.32-27.fc34                    updates"
    , "coreutils-common.x86_64                          8.32-27.fc34                    updates"
    ]

upgradeItems :: [Component.Item]
upgradeItems =
    [ Component.Item
        { Component.componentName    = Dnf.name
        , Component.itemName         = "coreutils.x86_64"
        , Component.installedVersion = Nothing
        , Component.availableVersion = Just "8.32-27.fc34"
        }
    , Component.Item
        { Component.componentName    = Dnf.name
        , Component.itemName         = "coreutils-common.x86_64"
        , Component.installedVersion = Nothing
        , Component.availableVersion = Just "8.32-27.fc34"
        }
    ]

testUpgrade :: TestTree
testUpgrade = testCase "upgrade" $
    ([], upgradeItems) @=? Dnf.parseItems upgradeOutput

------------------------------------------------------------------------------

errorOutput :: ByteString
errorOutput = BSL8.unlines
    [ "Fedora 34 openh264 (From Cisco) - x86_64                1.7 kB/s | 2.5 kB     00:01    "
    , "Fedora Modular 34 - x86_64                              2.6 MB/s | 4.9 MB     00:01    "
    , "Fedora Modular 34 - x86_64 - Updates                    2.3 MB/s | 4.2 MB     00:01    "
    , "Fedora 34 - x86_64 - Updates                            4.1 MB/s |  15 MB     00:03    "
    , "Fedora 34 - x86_64                                      6.6 MB/s |  74 MB     00:11    "
    , "Last metadata expiration check: 0:00:01 ago on Sat Jun 12 10:07:05 2021."
    , ""
    , "coreutils.x86_64                                 8.32-27.fc34                    updates"
    , "invalid line"
    , "coreutils-common.x86_64                          8.32-27.fc34                    updates"
    ]

testError :: TestTree
testError = testCase "error" $
    ([err], upgradeItems) @=? Dnf.parseItems errorOutput
  where
    err :: String
    err = "error parsing dnf line: invalid line"

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "LsUpg.Component.Dnf"
    [ testNone
    , testUpgrade
    , testError
    ]
