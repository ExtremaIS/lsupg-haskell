------------------------------------------------------------------------------
-- |
-- Module      : LsUpg.Component.Pacman
-- Description : pacman component
-- Copyright   : Copyright (c) 2021-2024 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module LsUpg.Component.Pacman
  ( -- * Constants
    name
  , component
    -- * Internal
  , parseItems
  ) where

-- https://hackage.haskell.org/package/attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as ABS8

-- https://hackage.haskell.org/package/base
import Control.Monad (mzero, unless)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe, isJust)
import System.IO (hPutStrLn)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
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
import LsUpg.Component (Component(Component), Options(Options, mDebugHandle))

------------------------------------------------------------------------------
-- $Constants

-- | Component name
--
-- @since 0.1.0.0
name :: Component.Name
name = $$(TTC.valid "pacman")

-- | Component API
--
-- @since 0.1.0.0
component :: Component
component = Component
    { Component.name        = name
    , Component.description = "Arch Linux packages"
    , Component.run         = run
    }

------------------------------------------------------------------------------
-- $Internal

-- | Run the component
run
  :: Options
  -> IO [Component.Item]
run Options{..} = fmap (fromMaybe []) . runMaybeT $ do
    syncDirExists <- lift $ Dir.doesDirectoryExist syncDir
    unless syncDirExists $ do
      putDebug $ syncDir ++ " not found (skipping)"
      mzero
    pacmanProgramExists <- fmap isJust . lift $ Dir.findExecutable "pacman"
    unless pacmanProgramExists $ do
      putDebug "pacman program not found (skipping)"
      mzero
    doUpdate
    getItems
  where
    syncDir :: FilePath
    syncDir = "/var/lib/pacman/sync"

    doUpdate :: MaybeT IO ()
    doUpdate = do
      putDebug "doUpdate: pacman -Sy"
      let stream = maybe TP.nullStream TP.useHandleOpen mDebugHandle
      lift
        . TP.runProcess_
        . TP.setStdin TP.nullStream
        . TP.setStdout stream
        . TP.setStdout stream
        $ TP.proc "pacman" ["-Sy"]

    getItems :: MaybeT IO [Component.Item]
    getItems = do
      putDebug "getItems: pacman -Qu"
      output <- lift
        . TP.readProcessStdout_
        . TP.setStdin TP.nullStream
        . TP.setStderr (maybe TP.nullStream TP.useHandleOpen mDebugHandle)
        $ TP.proc "pacman" ["-Qu"]
      lift $ maybe (return ()) (`BSL8.hPut` output) mDebugHandle
      putDebug "getItems: parseItems"
      let (errs, items) = parseItems output
      unless (null errs) $ mapM_ putDebug errs
      return items

    putDebug :: String -> MaybeT IO ()
    putDebug = case mDebugHandle of
      Just handle -> lift . hPutStrLn handle . ("[lsupg:pacman] " ++)
      Nothing     -> const $ return ()

------------------------------------------------------------------------------

-- | Parse items from command output
--
-- This internal function is only used for testing.
parseItems :: BSL8.ByteString -> ([String], [Component.Item])
parseItems = partitionEithers . map (parseLine . BSL8.toStrict) . BSL8.lines
  where
    -- /^([^ ]+) ([^ ]+) -> (.+)$/
    -- {{Name}} {{Installed}} -> {{Available}}
    -- pacman 5.2.2-3 -> 6.0.0-3
    parseLine :: BS.ByteString -> Either String Component.Item
    parseLine = parseOrError $ do
      itemName <- ABS8.takeWhile1 (/= ' ') <* ABS8.char ' '
      installedVersion <-
        Just <$> ABS8.takeWhile1 (/= ' ') <* ABS8.string " -> "
      availableVersion <- Just <$> ABS8.takeByteString
      return Component.Item
        { Component.componentName    = name
        , Component.itemName         = TTC.toT itemName
        , Component.installedVersion = TTC.toT <$> installedVersion
        , Component.availableVersion = TTC.toT <$> availableVersion
        }

    parseOrError :: ABS8.Parser a -> BS.ByteString -> Either String a
    parseOrError parser line
      = first (const $ "error parsing pacman line: " ++ TTC.toS line)
      $ ABS8.parseOnly parser line
