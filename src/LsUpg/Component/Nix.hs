------------------------------------------------------------------------------
-- |
-- Module      : LsUpg.Component.Nix
-- Description : nix component
-- Copyright   : Copyright (c) 2021-2024 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module LsUpg.Component.Nix
  ( -- * Constants
    name
  , component
  , defaultNixPath
    -- * Internal
  , parsePackages
  , resolveItems
  ) where

-- https://hackage.haskell.org/package/base
import Control.Monad (guard, mzero, unless)
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.List (sort, uncons)
import Data.Maybe (fromMaybe, isJust)
import System.IO (hPutStrLn)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- https://hackage.haskell.org/package/directory
import qualified System.Directory as Dir

-- https://hackage.haskell.org/package/text
import Data.Text (Text)

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- https://hackage.haskell.org/package/typed-process
import qualified System.Process.Typed as TP

-- https://hackage.haskell.org/package/unordered-containers
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HashSet

-- (lsupg)
import qualified LsUpg.Component as Component
import LsUpg.Component
  ( Component(Component), Options(Options, mDebugHandle, mNixPath)
  )

------------------------------------------------------------------------------
-- $Constants

-- | Component name
--
-- @since 0.1.0.0
name :: Component.Name
name = $$(TTC.valid "nix")

-- | Component API
--
-- @since 0.3.0.0
component :: Component
component = Component
    { Component.name        = name
    , Component.description = "Nix packages"
    , Component.run         = run
    }

-- | Default Nix path
--
-- @since 0.3.0.0
defaultNixPath :: FilePath
defaultNixPath = "/etc/nix/packages.nix"

------------------------------------------------------------------------------
-- $Internal

-- | Run the component
run
  :: Options
  -> IO [Component.Item]
run Options{..} = fmap (fromMaybe []) . runMaybeT $ do
    let nixPath = fromMaybe defaultNixPath mNixPath
    nixPathExists <- lift $ Dir.doesFileExist nixPath
    unless nixPathExists $ do
      putDebug $ nixPath ++ " not found (skipping)"
      mzero
    nixChannelProgramExists <- fmap isJust . lift $
      Dir.findExecutable "nix-channel"
    unless nixChannelProgramExists $ do
      putDebug "nix-channel not found (skipping)"
      mzero
    nixEnvProgramExists <- fmap isJust . lift $ Dir.findExecutable "nix-env"
    unless nixEnvProgramExists $ do
      putDebug "nix-env not found (skipping)"
      mzero
    doUpdate
    currentPackages <- getCurrentPackages
    targetPackages <- getTargetPackages nixPath
    return $ resolveItems currentPackages targetPackages
  where
    doUpdate :: MaybeT IO ()
    doUpdate = do
      putDebug "doUpdate: nix-channel --update"
      let stream = maybe TP.nullStream TP.useHandleOpen mDebugHandle
      lift
        . TP.runProcess_
        . TP.setStdin TP.nullStream
        . TP.setStdout stream
        . TP.setStderr stream
        $ TP.proc "nix-channel" ["--update"]

    getCurrentPackages :: MaybeT IO (HashMap Text Text)
    getCurrentPackages = do
      putDebug "getCurrentPackages: nix-env -q"
      output <- lift
        . TP.readProcessStdout_
        . TP.setStdin TP.nullStream
        . TP.setStderr (maybe TP.nullStream TP.useHandleOpen mDebugHandle)
        $ TP.proc "nix-env" ["-q"]
      lift $ maybe (return ()) (`BSL8.hPut` output) mDebugHandle
      putDebug "getCurrentPackages: parsePackages"
      let (errs, packages) = parsePackages output
      unless (null errs) $ mapM_ putDebug errs
      return packages

    getTargetPackages :: FilePath -> MaybeT IO (HashMap Text Text)
    getTargetPackages nixPath = do
      putDebug $ "getTargetPackages: nix-env -qaf " ++ nixPath
      output <- lift
        . TP.readProcessStdout_
        . TP.setStdin TP.nullStream
        . TP.setStderr (maybe TP.nullStream TP.useHandleOpen mDebugHandle)
        $ TP.proc "nix-env" ["-qaf", nixPath]
      lift $ maybe (return ()) (`BSL8.hPut` output) mDebugHandle
      putDebug "getTargetPackages: parsePackages"
      let (errs, packages) = parsePackages output
      unless (null errs) $ mapM_ putDebug errs
      return packages

    putDebug :: String -> MaybeT IO ()
    putDebug = case mDebugHandle of
      Just handle -> lift . hPutStrLn handle . ("[lsupg:nix] " ++)
      Nothing     -> const $ return ()
{-# ANN run ("HLint: ignore Use <$>" :: String) #-}

------------------------------------------------------------------------------

-- | Parse packages from command output
--
-- This internal function is only exported for testing.
parsePackages :: BSL8.ByteString -> ([String], HashMap Text Text)
parsePackages
    = fmap HashMap.fromList
    . partitionEithers
    . map (parseLine . BSL8.toStrict)
    . BSL8.lines
  where
    parseLine :: BS.ByteString -> Either String (Text, Text)
    parseLine line =
      maybe (Left $ "error parsing nix line: " ++ TTC.toS line) Right $ do
        -- the first part is a name part even if it begins with a digit
        (namePart, parts) <- uncons $ BS8.split '-' line
        let (nameParts, versionParts) =
              break (maybe True (isDigit . fst) . BS8.uncons) parts
        guard . not $ null versionParts
        let joinParts = TTC.toT . BS8.intercalate "-"
        return (joinParts (namePart : nameParts), joinParts versionParts)

------------------------------------------------------------------------------

-- | Resolve upgrade items
--
-- This internal function is only exported for testing.
resolveItems :: HashMap Text Text -> HashMap Text Text -> [Component.Item]
resolveItems currentPackages targetPackages =
    [ Component.Item
        { Component.componentName    = name
        , Component.itemName         = itemName
        , Component.installedVersion = HashMap.lookup itemName currentPackages
        , Component.availableVersion = HashMap.lookup itemName targetPackages
        }
    | itemName <- sort . HashSet.toList $ HashSet.union
        (HashMap.keysSet currentPackages)
        (HashMap.keysSet targetPackages)
    ]
