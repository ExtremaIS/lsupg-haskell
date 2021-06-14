------------------------------------------------------------------------------
-- |
-- Module      : LsUpg.Component.Nix
-- Description : nix component
-- Copyright   : Copyright (c) 2021 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LsUpg.Component.Nix
  ( -- * Constants
    name
  , component
    -- * Internal
  , parseItems
  ) where

-- https://hackage.haskell.org/package/attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as ABS8

-- https://hackage.haskell.org/package/base
import Control.Monad (guard, mzero, unless, void)
import Data.Bifunctor (bimap, first)
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe, isJust)
import System.IO (Handle, hPutStrLn)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- https://hackage.haskell.org/package/directory
import qualified System.Directory as Dir

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- https://hackage.haskell.org/package/typed-process
import qualified System.Process.Typed as TP

-- (lsupg)
import qualified LsUpg.Component as Component
import LsUpg.Component (Component(Component))

------------------------------------------------------------------------------
-- $Constants

-- | Component name
--
-- @since 0.1.0.0
name :: Component.Name
name = $$(TTC.valid "nix")

-- | Component API
--
-- @since 0.1.0.0
component :: Component
component = Component
    { Component.name        = name
    , Component.description = "Nix packages"
    , Component.run         = run
    }

------------------------------------------------------------------------------
-- $Internal

-- | Run the component
run
  :: Maybe Handle  -- ^ optional debug handle
  -> IO [Component.Item]
run mDebugHandle = fmap (fromMaybe []) . runMaybeT $ do
    nixDirExists <- lift $ Dir.doesDirectoryExist nixDir
    unless nixDirExists $ do
      putDebug $ nixDir ++ " not found (skipping)"
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
    getItems
  where
    nixDir :: FilePath
    nixDir = "/nix"

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

    getItems :: MaybeT IO [Component.Item]
    getItems = do
      putDebug "getItems: nix-env --upgrade --dry-run"
      output <- lift
        . TP.readProcessStderr_
        . TP.setStdin TP.nullStream
        . TP.setStdout (maybe TP.nullStream TP.useHandleOpen mDebugHandle)
        $ TP.proc "nix-env" ["--upgrade", "--dry-run"]
      lift $ maybe (return ()) (`BSL8.hPut` output) mDebugHandle
      putDebug "getItems: parseItems"
      let (errs, items) = parseItems output
      unless (null errs) $ mapM_ putDebug errs
      return items

    putDebug :: String -> MaybeT IO ()
    putDebug = case mDebugHandle of
      Just handle -> lift . hPutStrLn handle . ("[lsupg:nix] " ++)
      Nothing     -> const $ return ()

------------------------------------------------------------------------------

-- | Parse items from command output
--
-- This internal function is only used for testing.
parseItems :: BSL8.ByteString -> ([String], [Component.Item])
parseItems
    = partitionEithers
    . map parseLine
    . filter (BS.isPrefixOf "upgrading ")
    . map BSL8.toStrict
    . BSL8.lines
  where
    -- /^upgrading '([^']+)' to '([^']+)'$/
    -- upgrading '{{NAME}}-{{INSTALLED}}' to '{{NAME}}-{{AVAILABLE}}'
    -- upgrading 'nix-2.3.11' to 'nix-2.3.12'
    parseLine :: BS.ByteString -> Either String Component.Item
    parseLine = parseOrError $ do
      void $ ABS8.string "upgrading '"
      (installedName, installedVersion) <- parseNameAndVersion <$>
        ABS8.takeWhile1 (/= '\'') <* ABS8.string "' to '"
      (availableName, availableVersion) <- parseNameAndVersion <$>
        ABS8.takeWhile1 (/= '\'') <* ABS8.char '\''
      ABS8.endOfInput
      guard $ installedName == availableName
      return Component.Item
        { Component.componentName    = name
        , Component.itemName         = TTC.toT installedName
        , Component.installedVersion = Just $ TTC.toT installedVersion
        , Component.availableVersion = Just $ TTC.toT availableVersion
        }

    parseNameAndVersion :: BS.ByteString -> (BS.ByteString, BS.ByteString)
    parseNameAndVersion
      = bimap (BS8.intercalate "-") (BS8.intercalate "-")
      . break (maybe True (isDigit . fst) . BS8.uncons)
      . BS8.split '-'

    parseOrError :: ABS8.Parser a -> BS.ByteString -> Either String a
    parseOrError parser line
      = first (const $ "error parsing nix line: " ++ TTC.toS line)
      $ ABS8.parseOnly parser line
