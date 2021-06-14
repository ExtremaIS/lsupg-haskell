------------------------------------------------------------------------------
-- |
-- Module      : LsUpg.Component.Apt
-- Description : apt component
-- Copyright   : Copyright (c) 2021 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LsUpg.Component.Apt
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
import System.IO (Handle, hPutStrLn)

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
import LsUpg.Component (Component(Component))

------------------------------------------------------------------------------
-- $Constants

-- | Component name
--
-- @since 0.1.0.0
name :: Component.Name
name = $$(TTC.valid "apt")

-- | Component API
--
-- @since 0.1.0.0
component :: Component
component = Component
    { Component.name        = name
    , Component.description = "Debian packages"
    , Component.run         = run
    }

------------------------------------------------------------------------------
-- $Internal

-- | Run the component
run
  :: Maybe Handle  -- ^ optional debug handle
  -> IO [Component.Item]
run mDebugHandle = fmap (fromMaybe []) . runMaybeT $ do
    listsDirExists <- lift $ Dir.doesDirectoryExist listsDir
    unless listsDirExists $ do
      putDebug $ listsDir ++ " not found (skipping)"
      mzero
    aptProgramExists <- fmap isJust . lift $ Dir.findExecutable "apt"
    unless aptProgramExists $ do
      putDebug "apt program not found (skipping)"
      mzero
    doUpdate
    getItems
  where
    listsDir :: FilePath
    listsDir = "/var/lib/apt/lists"

    doUpdate :: MaybeT IO ()
    doUpdate = do
      putDebug "doUpdate: apt-get update"
      let stream = maybe TP.nullStream TP.useHandleOpen mDebugHandle
      lift
        . TP.runProcess_
        . TP.setStdin TP.nullStream
        . TP.setStdout stream
        . TP.setStderr stream
        $ TP.proc "apt-get" ["update"]

    getItems :: MaybeT IO [Component.Item]
    getItems = do
      putDebug "getItems: apt-get dist-upgrade -s"
      output <- lift
        . TP.readProcessStdout_
        . TP.setStdin TP.nullStream
        . TP.setStderr (maybe TP.nullStream TP.useHandleOpen mDebugHandle)
        $ TP.proc "apt-get" ["dist-upgrade", "-s"]
      lift $ maybe (return ()) (`BSL8.hPut` output) mDebugHandle
      putDebug "getItems: parseItems"
      let (errs, items) = parseItems output
      unless (null errs) $ mapM_ putDebug errs
      return items

    putDebug :: String -> MaybeT IO ()
    putDebug = case mDebugHandle of
      Just handle -> lift . hPutStrLn handle . ("[lsupg:apt] " ++)
      Nothing     -> const $ return ()

------------------------------------------------------------------------------

-- | Parse items from command output
--
-- This internal function is only used for testing.
parseItems :: BSL8.ByteString -> ([String], [Component.Item])
parseItems
    = partitionEithers
    . map parseLine
    . filter (BS.isPrefixOf "Inst ")
    . map BSL8.toStrict
    . BSL8.lines
  where
    -- /^Inst ([^ ]+) (?:\[([^]]+)\])? \(([^ ]+) [^)]+\)$/
    -- Inst {{Name}} ([{{Installed}}])? ({{Available ...)
    -- Inst liblz4-1 [1.8.3-1] (1.8.3-1+deb10u1 Debian-Security:10/stable [amd64])
    parseLine :: BS.ByteString -> Either String Component.Item
    parseLine = parseOrError $ do
      itemName <-
        ABS8.string "Inst " *> ABS8.takeWhile1 (/= ' ') <* ABS8.char ' '
      installedVersion <- (ABS8.option Nothing . fmap Just)
        (ABS8.char '[' *> ABS8.takeWhile1 (/= ']') <* ABS8.string "] ")
      availableVersion <- Just <$> (ABS8.char '(' *> ABS8.takeWhile1 (/= ' '))
      ABS8.takeWhile1 (/= ')') *> ABS8.char ')' *> ABS8.endOfInput
      return Component.Item
        { Component.componentName    = name
        , Component.itemName         = TTC.toT itemName
        , Component.installedVersion = TTC.toT <$> installedVersion
        , Component.availableVersion = TTC.toT <$> availableVersion
        }

    parseOrError :: ABS8.Parser a -> BS.ByteString -> Either String a
    parseOrError parser line
      = first (const $ "error parsing apt line: " ++ TTC.toS line)
      $ ABS8.parseOnly parser line
