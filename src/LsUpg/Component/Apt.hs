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
import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import System.IO (Handle, hPutStrLn)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL8

-- https://hackage.haskell.org/package/directory
import qualified System.Directory as Dir

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)

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
    { Component.name      = name
    , Component.getStatus = getStatus
    , Component.doUpdate  = doUpdate
    , Component.getItems  = getItems
    }

------------------------------------------------------------------------------
-- $Internal

putDebug
  :: Maybe Handle
  -> String
  -> IO ()
putDebug Nothing _message = return ()
putDebug (Just handle) message = hPutStrLn handle $ "[lsupg:apt] " ++ message

------------------------------------------------------------------------------

getStatus
  :: Maybe Handle
  -> IO Component.Status
getStatus mHandle = fmap (either id id) . runExceptT $ do
    exists <- lift $ Dir.doesDirectoryExist listsDir
    unless exists $ do
      lift $ putDebug mHandle "getStatus: NotFound"
      throwE Component.NotFound
    count <- lift $ length <$> Dir.listDirectory listsDir
    unless (count > 0) $ do
      lift $ putDebug mHandle "getStatus: Empty"
      throwE Component.Empty
    lift $ do
      time <- Dir.getModificationTime listsDir
      putDebug mHandle $ "getStatus: Updated " ++ show time
      return $ Component.Updated time
  where
    listsDir :: FilePath
    listsDir = "/var/lib/apt/lists"

------------------------------------------------------------------------------

doUpdate
  :: Maybe Handle
  -> IO ()
doUpdate mHandle = do
    putDebug mHandle "doUpdate: apt-get update"
    let stream = maybe TP.nullStream TP.useHandleOpen mHandle
    TP.runProcess_
      . TP.setStdin TP.nullStream
      . TP.setStdout stream
      . TP.setStderr stream
      $ TP.proc "apt-get" ["update"]

------------------------------------------------------------------------------

getItems
  :: Maybe Handle
  -> IO [Component.Item]
getItems mHandle = do
    putDebug mHandle "getItems: apt-get dist-upgrade -s"
    output <- TP.readProcessStdout_
      . TP.setStdin TP.nullStream
      . TP.setStderr (maybe TP.nullStream TP.useHandleOpen mHandle)
      $ TP.proc "apt-get" ["dist-upgrade", "-s"]
    maybe (return ()) (`BSL8.hPut` output) mHandle
    let (errs, items) = parseItems output
    unless (null errs) $ do
      putDebug mHandle "getItems: parseItems"
      mapM_ (putDebug mHandle) errs
    return items

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
