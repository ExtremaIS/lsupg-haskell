------------------------------------------------------------------------------
-- |
-- Module      : LsUpg.Component.Dnf
-- Description : dnf component
-- Copyright   : Copyright (c) 2021 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LsUpg.Component.Dnf
  ( -- * Constants
    name
  , component
    -- * Internal
  , parseItems
  , parseInfo
  ) where

-- https://hackage.haskell.org/package/attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as ABS8

-- https://hackage.haskell.org/package/base
import Control.Monad (mzero, unless, void)
import Data.Bifunctor (first)
import Data.Either (fromRight, partitionEithers)
import Data.Maybe (fromMaybe, isJust)
import System.IO (Handle, hPutStrLn)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- https://hackage.haskell.org/package/directory
import qualified System.Directory as Dir

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

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
name = $$(TTC.valid "dnf")

-- | Component API
--
-- @since 0.1.0.0
component :: Component
component = Component
    { Component.name        = name
    , Component.description = "Fedora packages"
    , Component.run         = run
    }

------------------------------------------------------------------------------
-- $Internal

-- | Run the component
run
  :: Maybe Handle  -- ^ optional debug handle
  -> IO [Component.Item]
run mDebugHandle = fmap (fromMaybe []) . runMaybeT $ do
    dnfDirExists <- lift $ Dir.doesDirectoryExist dnfDir
    unless dnfDirExists $ do
      putDebug $ dnfDir ++ " not found (skipping)"
      mzero
    dnfProgramExists <- fmap isJust . lift $ Dir.findExecutable "dnf"
    unless dnfProgramExists $ do
      putDebug "dnf program not found (skipping)"
      mzero
    mapM getInstalledVersion =<< getItems
  where
    dnfDir :: FilePath
    dnfDir = "/var/lib/dnf"

    getItems :: MaybeT IO [Component.Item]
    getItems = do
      putDebug "getItems: dnf check-update"
      (_exitCode, output) <- lift
        . TP.readProcessStdout
        . TP.setStdin TP.nullStream
        . TP.setStderr (maybe TP.nullStream TP.useHandleOpen mDebugHandle)
        $ TP.proc "dnf" ["check-update"]
      lift $ maybe (return ()) (`BSL8.hPut` output) mDebugHandle
      putDebug "getItems: parseItems"
      let (errs, items) = parseItems output
      unless (null errs) $ mapM_ putDebug errs
      return items

    getInstalledVersion :: Component.Item -> MaybeT IO Component.Item
    getInstalledVersion item = do
      let itemName = Component.itemName item
          dnfName  = TTC.toS $ T.takeWhile (/= '.') itemName
      putDebug $ "getInstalledVersion: dnf info --installed " ++ dnfName
      output <- lift
        . TP.readProcessStdout_
        . TP.setStdin TP.nullStream
        . TP.setStderr (maybe TP.nullStream TP.useHandleOpen mDebugHandle)
        $ TP.proc "dnf" ["info", "--installed", dnfName]
      lift $ maybe (return ()) (`BSL8.hPut` output) mDebugHandle
      putDebug "getInstalledVersion: parseInfo"
      return $ case lookup itemName (parseInfo output) of
        Just installedVersion -> item
          { Component.installedVersion = Just installedVersion
          }
        Nothing -> item

    putDebug :: String -> MaybeT IO ()
    putDebug = case mDebugHandle of
      Just handle -> lift . hPutStrLn handle . ("[lsupg:dnf] " ++)
      Nothing     -> const $ return ()

------------------------------------------------------------------------------

-- | Parse items from command output
--
-- This internal function is only used for testing.
parseItems :: BSL8.ByteString -> ([String], [Component.Item])
parseItems
    = partitionEithers
    . map parseLine
    . dropWhile BS.null
    . dropWhile (not . BS.null)
    . map BSL8.toStrict
    . BSL8.lines
  where
    -- /^([^.]+)\.([^ ]+) +(?:([^:]+):)?([^-]+)-([^ ]+) +[^ ]+$/
    -- {{Name}}.{{Architecture}}  ({{Epoch}}:)?{{Version}}-{{Release}}  ...
    -- coreutils.x86_64    8.32-27.fc34    updates
    parseLine :: BS.ByteString -> Either String Component.Item
    parseLine = parseOrError $ do
      -- itemName: {{Name}}.{{Architecture}}
      itemName <- ABS8.takeWhile1 (/= ' ') <* ABS8.takeWhile1 (== ' ')
      -- availableVersion: ({{Epoch}}:)?{{Version}}-{{Release}}
      availableVersion <- Just <$> ABS8.takeWhile1 (/= ' ')
      ABS8.takeWhile1 (== ' ') *> ABS8.takeWhile1 (/= ' ') *> ABS8.endOfInput
      return Component.Item
        { Component.componentName    = name
        , Component.itemName         = TTC.toT itemName
        , Component.installedVersion = Nothing
        , Component.availableVersion = TTC.toT <$> availableVersion
        }

    parseOrError :: ABS8.Parser a -> BS.ByteString -> Either String a
    parseOrError parser line
      = first (const $ "error parsing dnf line: " ++ TTC.toS line)
      $ ABS8.parseOnly parser line

------------------------------------------------------------------------------

-- | Parse package information from command output
--
-- This internal function is only used for testing.
parseInfo :: BSL8.ByteString -> [(Text, Text)]
parseInfo = parseOrEmpty $ do
    void $ ABS8.string "Installed Packages" *> ABS8.endOfLine
    flip ABS8.sepBy ABS8.endOfLine $ do
      itemName <- parseLineFor "Name"
      epoch    <- ABS8.option Nothing $ Just <$> parseLineFor "Epoch"
      version  <- parseLineFor "Version"
      release  <- parseLineFor "Release"
      arch     <- parseLineFor "Architecture"
      void $ ABS8.manyTill takeRestOfLine ABS8.endOfLine
      ABS8.endOfInput
      return
        ( itemName <> "." <> arch
        , maybe "" (<> ":") epoch <> version <> "-" <> release
        )
  where
    takeRestOfLine :: ABS8.Parser BS.ByteString
    takeRestOfLine = ABS8.takeTill (== '\n') <* ABS8.endOfLine

    parseLineFor :: BS.ByteString -> ABS8.Parser Text
    parseLineFor key = fmap TTC.toT $ ABS8.string key
      *> ABS8.takeWhile1 (== ' ')
      *> ABS8.string ": "
      *> takeRestOfLine

    parseOrEmpty :: ABS8.Parser [a] -> BSL8.ByteString -> [a]
    parseOrEmpty parser = fromRight [] . ABS8.parseOnly parser . TTC.toBS
