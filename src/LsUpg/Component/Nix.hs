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
import Control.Monad (guard, unless, void)
import Data.Bifunctor (bimap, first)
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import System.IO (Handle, hPutStrLn)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- https://hackage.haskell.org/package/directory
import qualified System.Directory as Dir

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- https://hackage.haskell.org/package/typed-process
import qualified System.Process.Typed as TP

-- (lsupg)
import qualified LsUpg.Component as Component
import LsUpg.Component (Component(Component))

------------------------------------------------------------------------------
-- $Constants

name :: Component.Name
name = $$(TTC.valid "nix")

component :: Component
component = Component
    { Component.name = name
    , Component.run  = run
    }

------------------------------------------------------------------------------
-- $Internal

run
  :: Maybe Handle
  -> IO [Component.Item]
run mDebugHandle = do
    exists <- Dir.doesDirectoryExist nixDir
    if exists
      then do
        doUpdate
        getItems
      else do
        putDebug "not found (skipping)"
        return []
  where
    nixDir :: FilePath
    nixDir = "/nix"

    doUpdate :: IO ()
    doUpdate = do
      putDebug "doUpdate: nix-channel --update"
      let stream = maybe TP.nullStream TP.useHandleOpen mDebugHandle
      TP.runProcess_
        . TP.setStdin TP.nullStream
        . TP.setStdout stream
        . TP.setStderr stream
        $ TP.proc "nix-channel" ["--update"]

    getItems :: IO [Component.Item]
    getItems = do
      putDebug "getItems: nix-env --upgrade --dry-run"
      output <- TP.readProcessStderr_
        . TP.setStdin TP.nullStream
        . TP.setStdout (maybe TP.nullStream TP.useHandleOpen mDebugHandle)
        $ TP.proc "nix-env" ["--upgrade", "--dry-run"]
      maybe (return ()) (`BSL8.hPut` output) mDebugHandle
      putDebug "getItems: parseItems"
      let (errs, items) = parseItems output
      unless (null errs) $ mapM_ putDebug errs
      return items

    putDebug :: String -> IO ()
    putDebug = case mDebugHandle of
      Just handle -> hPutStrLn handle . ("[lsupg:nix] " ++)
      Nothing     -> const $ return ()

------------------------------------------------------------------------------

parseItems :: BSL8.ByteString -> ([String], [Component.Item])
parseItems
    = partitionEithers
    . map parseLine
    . filter (BS.isPrefixOf "upgrading ")
    . map BSL8.toStrict
    . BSL8.lines
  where
    parseLine :: BS.ByteString -> Either String Component.Item
    parseLine = parseOrError $ do
      void $ ABS8.string "upgrading '"
      (installedName, installedVersion) <- parseNameAndVersion <$>
        ABS8.takeTill (== '\'') <* ABS8.string "' to '"
      (availableName, availableVersion) <- parseNameAndVersion <$>
        ABS8.takeTill (== '\'') <* ABS8.char '\''
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
