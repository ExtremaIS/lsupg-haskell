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

testParseItemsNone :: TestTree
testParseItemsNone = testCase "none" $
    ([], []) @=? Dnf.parseItems output
  where
    output :: ByteString
    output = BSL8.unlines
      [ "Last metadata expiration check: 1:36:09 ago on Sun Jun 13 23:12:48 2021."
      ]

------------------------------------------------------------------------------

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

testParseItemsUpgrade :: TestTree
testParseItemsUpgrade = testCase "upgrade" $
    ([], upgradeItems) @=? Dnf.parseItems output
  where
    output :: ByteString
    output = BSL8.unlines
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

------------------------------------------------------------------------------

testParseItemsError :: TestTree
testParseItemsError = testCase "error" $
    ([err], upgradeItems) @=? Dnf.parseItems output
  where
    err :: String
    err = "error parsing dnf line: invalid line"

    output :: ByteString
    output = BSL8.unlines
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

------------------------------------------------------------------------------

testParseInfoNoEpoch :: TestTree
testParseInfoNoEpoch = testCase "no-epoch" $
    [("coreutils.x86_64", "8.32-24.fc34")] @=? Dnf.parseInfo output
  where
    output :: ByteString
    output = BSL8.unlines
      [ "Installed Packages"
      , "Name         : coreutils"
      , "Version      : 8.32"
      , "Release      : 24.fc34"
      , "Architecture : x86_64"
      , "Size         : 5.8 M"
      , "Source       : coreutils-8.32-24.fc34.src.rpm"
      , "Repository   : @System"
      , "From repo    : koji-override-0"
      , "Summary      : A set of basic GNU tools commonly used in shell scripts"
      , "URL          : https://www.gnu.org/software/coreutils/"
      , "License      : GPLv3+"
      , "Description  : These are the GNU core utilities.  This package is the combination of"
      , "             : the old GNU fileutils, sh-utils, and textutils packages."
      , ""
      ]

------------------------------------------------------------------------------

testParseInfoEpoch :: TestTree
testParseInfoEpoch = testCase "epoch" $
    [("vim-minimal.x86_64", "2:8.2.2846-1.fc34")] @=? Dnf.parseInfo output
  where
    output :: ByteString
    output = BSL8.unlines
      [ "Installed Packages"
      , "Name         : vim-minimal"
      , "Epoch        : 2"
      , "Version      : 8.2.2846"
      , "Release      : 1.fc34"
      , "Architecture : x86_64"
      , "Size         : 1.4 M"
      , "Source       : vim-8.2.2846-1.fc34.src.rpm"
      , "Repository   : @System"
      , "From repo    : koji-override-0"
      , "Summary      : A minimal version of the VIM editor"
      , "URL          : http://www.vim.org/"
      , "License      : Vim and MIT"
      , "Description  : VIM (VIsual editor iMproved) is an updated and improved version of the"
      , "             : vi editor.  Vi was the first real screen-based editor for UNIX, and is"
      , "             : still very popular.  VIM improves on vi by adding new features:"
      , "             : multiple windows, multi-level undo, block highlighting and more. The"
      , "             : vim-minimal package includes a minimal version of VIM, providing"
      , "             : the commands vi, view, ex, rvi, and rview. NOTE: The online help is"
      , "             : only available when the vim-common package is installed."
      , ""
      ]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "LsUpg.Component.Dnf"
    [ testGroup "parseItems"
        [ testParseItemsNone
        , testParseItemsUpgrade
        , testParseItemsError
        ]
    , testGroup "parseInfo"
        [ testParseInfoNoEpoch
        , testParseInfoEpoch
        ]
    ]
