{-# LANGUAGE LambdaCase #-}

module LsUpg
  ( -- * Constants
    version
  , allComponents
    -- * Types
  , OutputFormat(..)
    -- * API
  , runAllComponents
  , runComponent
  ) where

-- https://hackage.haskell.org/package/aeson
import qualified Data.Aeson as A

-- https://hackage.haskell.org/package/base
import Control.Monad (forM)
import Data.List (transpose)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Version (showVersion)
import System.IO (Handle, hPutStrLn)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- https://hackage.haskell.org/package/cassava
import qualified Data.Csv as CSV

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

-- https://hackage.haskell.org/package/time
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- https://hackage.haskell.org/package/yaml
import qualified Data.Yaml as Yaml

-- (lsupg)
import qualified LsUpg.Component as Component
import LsUpg.Component (Component)
import qualified LsUpg.Component.Apt

-- (lsupg:cabal)
import qualified Paths_lsupg as Project

------------------------------------------------------------------------------
-- $Constants

-- | lsupg version string (\"@lsupg-haskell X.X.X@\")
version :: String
version = "lsupg-haskell " ++ showVersion Project.version

------------------------------------------------------------------------------

-- | All components
--
-- Components are processed in the defined order.
allComponents :: [Component]
allComponents =
    [ LsUpg.Component.Apt.component
    ]

------------------------------------------------------------------------------
-- $Types

data OutputFormat
  = OutputHuman
  | OutputCSV
  | OutputJSON
  | OutputYAML
  deriving (Bounded, Enum, Eq, Ord, Show)

instance TTC.Parse OutputFormat where
  parse = TTC.parseEnum' "output format" True False

instance TTC.Render OutputFormat where
  render = TTC.fromS . \case
    OutputHuman -> "human"
    OutputCSV   -> "csv"
    OutputJSON  -> "json"
    OutputYAML  -> "yaml"

------------------------------------------------------------------------------
-- $API

runAllComponents
  :: Handle
  -> Maybe Handle
  -> Maybe NominalDiffTime
  -> OutputFormat
  -> IO Bool
runAllComponents = run allComponents

------------------------------------------------------------------------------

runComponent
  :: Component.Name
  -> Handle
  -> Maybe Handle
  -> Maybe NominalDiffTime
  -> OutputFormat
  -> IO (Maybe Bool)
runComponent name outHandle mDebugHandle mUpdateDuration outputFormat =
    case lookupComponent name of
      Just component -> Just <$>
        run [component] outHandle mDebugHandle mUpdateDuration outputFormat
      Nothing -> return Nothing

------------------------------------------------------------------------------
-- $Internal

lookupComponent
  :: Component.Name
  -> Maybe Component
lookupComponent name =
    -- O(n) performance; improve if number of components increases?
    listToMaybe
      [ component
      | component <- allComponents
      , Component.name component == name
      ]

------------------------------------------------------------------------------

putDebug
  :: Maybe Handle
  -> String
  -> IO ()
putDebug Nothing       = const $ return ()
putDebug (Just handle) = hPutStrLn handle . ("[lsupg] " ++)

------------------------------------------------------------------------------

run
  :: [Component]
  -> Handle
  -> Maybe Handle
  -> Maybe NominalDiffTime
  -> OutputFormat
  -> IO Bool
run components outHandle mDebugHandle mUpdateDuration outputFormat = do
    currentTime <- getCurrentTime
    putDebug mDebugHandle $ "run: " ++ show currentTime
    let shouldUpdate updateTime = case mUpdateDuration of
          Just dur -> currentTime `diffUTCTime` updateTime >= dur
          Nothing  -> False
    items <- fmap concat . forM components $ \component ->
      Component.run component mDebugHandle shouldUpdate
    putItems items
    return . not $ null items
  where
    putItems :: [Component.Item] -> IO ()
    putItems items = case outputFormat of
      OutputHuman -> TLIO.hPutStr outHandle $ table
        [ [ TTC.render $ Component.componentName item
          , Component.itemName item
          , fromMaybe "" $ Component.installedVersion item
          , fromMaybe "" $ Component.availableVersion item
          ]
        | item <- items
        ]
      OutputCSV   -> BSL.hPutStr outHandle $ CSV.encode items
      OutputJSON  -> BSL.hPutStr outHandle $ A.encode items
      OutputYAML  -> BS.hPutStr outHandle $ Yaml.encode items

------------------------------------------------------------------------------

table :: [[T.Text]] -> TL.Text
table rows = TL.unlines
    [ TL.fromStrict . T.stripEnd $ T.concat
        [ T.justifyLeft len ' ' t
        | (len, t) <- zip lens cols
        ]
    | cols <- rows
    ]
  where
    lens :: [Int]
    lens = map ((+) 2 . sum . map T.length) $ transpose rows
