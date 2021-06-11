{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LsUpg.Component.Apt
  ( -- * Component
    component
    -- * Internal
  , parseItems
  ) where

-- https://hackage.haskell.org/package/attoparsec
import qualified Data.Attoparsec.ByteString.Char8 as ABS8

-- https://hackage.haskell.org/package/base
import Control.Monad (unless, when)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import System.IO (Handle, hPutStrLn)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- https://hackage.haskell.org/package/directory
import qualified System.Directory as Dir

-- https://hackage.haskell.org/package/time
import Data.Time.Clock (UTCTime)

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
name = $$(TTC.valid "apt")

component :: Component
component = Component
    { Component.name = name
    , Component.run  = run
    }

------------------------------------------------------------------------------
-- $Internal

data Status
  = NotFound
  | Empty
  | Updated !UTCTime
  deriving Show

------------------------------------------------------------------------------

run
  :: Maybe Handle
  -> (UTCTime -> Bool)
  -> IO [Component.Item]
run mDebugHandle shouldUpdate = do
    status <- getStatus
    putDebug $ "getStatus: " ++ show status
    case plan status of
      Nothing -> return []
      Just update -> do
        when update doUpdate
        getItems
  where
    listsDir :: FilePath
    listsDir = "/var/lib/apt/lists"

    getStatus :: IO Status
    getStatus = do
      exists <- Dir.doesDirectoryExist listsDir
      if exists
        then do
          count <- length <$> Dir.listDirectory listsDir
          if count > 0
            then Updated <$> Dir.getModificationTime listsDir
            else return Empty
        else return NotFound

    plan :: Status -> Maybe Bool
    plan NotFound             = Nothing
    plan Empty                = Just True
    plan (Updated updateTime) = Just $ shouldUpdate updateTime

    doUpdate :: IO ()
    doUpdate = do
      putDebug "doUpdate: apt-get update"
      let stream = maybe TP.nullStream TP.useHandleOpen mDebugHandle
      TP.runProcess_
        . TP.setStdin TP.nullStream
        . TP.setStdout stream
        . TP.setStderr stream
        $ TP.proc "apt-get" ["update"]

    getItems :: IO [Component.Item]
    getItems = do
      putDebug "getItems: apt-get dist-upgrade -s"
      output <- TP.readProcessStdout_
        . TP.setStdin TP.nullStream
        . TP.setStderr (maybe TP.nullStream TP.useHandleOpen mDebugHandle)
        $ TP.proc "apt-get" ["dist-upgrade", "-s"]
      maybe (return ()) (`BSL8.hPut` output) mDebugHandle
      let (errs, items) = parseItems output
      unless (null errs) $ do
        putDebug "getItems: parseItems"
        mapM_ putDebug errs
      return items

    putDebug :: String -> IO ()
    putDebug = case mDebugHandle of
      Just handle -> hPutStrLn handle . ("[lsupg:apt] " ++)
      Nothing     -> const $ return ()

------------------------------------------------------------------------------

parseItems :: BSL8.ByteString -> ([String], [Component.Item])
parseItems
    = partitionEithers
    . map parseLine
    . filter (BS.isPrefixOf "Inst ")
    . map BSL8.toStrict
    . BSL8.lines
  where
    parseLine :: BS.ByteString -> Either String Component.Item
    parseLine = parseOrError $ do
      itemName <-
        ABS8.string "Inst " *> ABS8.takeTill (== ' ') <* ABS8.char ' '
      installedVersion <- (ABS8.option Nothing . fmap Just)
        (ABS8.char '[' *> ABS8.takeTill (== ']') <* ABS8.string "] ")
      availableVersion <- Just <$> (ABS8.char '(' *> ABS8.takeTill (== ' '))
      ABS8.takeTill (== ')') *> ABS8.char ')' *> ABS8.endOfInput
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
