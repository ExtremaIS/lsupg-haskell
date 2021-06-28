------------------------------------------------------------------------------
-- |
-- Module      : LsUpg
-- Description : API
-- Copyright   : Copyright (c) 2021 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

module LsUpg
  ( -- * Constants
    version
  , allComponents
    -- * Types
  , OutputFormat(..)
    -- * API
  , lookupComponent
  , run
  , runAll
  ) where

-- https://hackage.haskell.org/package/aeson
import qualified Data.Aeson as A

-- https://hackage.haskell.org/package/base
import Control.Monad (forM)
import Data.List (transpose)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import System.IO (Handle)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- https://hackage.haskell.org/package/cassava
import qualified Data.Csv as CSV

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- https://hackage.haskell.org/package/unodered-containers
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

-- https://hackage.haskell.org/package/yaml
import qualified Data.Yaml as Yaml

-- (lsupg)
import qualified LsUpg.Component as Component
import LsUpg.Component (Component)
import qualified LsUpg.Component.Apk
import qualified LsUpg.Component.Apt
import qualified LsUpg.Component.Dnf
import qualified LsUpg.Component.Nix
import qualified LsUpg.Component.Pacman

-- (lsupg:cabal)
import qualified Paths_lsupg as Project

------------------------------------------------------------------------------
-- $Constants

-- | lsupg version string (\"@lsupg-haskell X.X.X.X@\")
--
-- @since 0.1.0.0
version :: String
version = "lsupg-haskell " ++ showVersion Project.version

------------------------------------------------------------------------------

-- | All components
--
-- Components are processed in the defined order.
--
-- @since 0.1.0.0
allComponents :: [Component]
allComponents =
    [ LsUpg.Component.Apk.component
    , LsUpg.Component.Apt.component
    , LsUpg.Component.Dnf.component
    , LsUpg.Component.Nix.component
    , LsUpg.Component.Pacman.component
    ]

-- | Map from component name to component
allComponentsMap :: HashMap Component.Name Component
allComponentsMap = HashMap.fromList
    [ (Component.name component, component)
    | component <- allComponents
    ]

------------------------------------------------------------------------------
-- $Types

-- | Output format
--
-- This sum type defines the supported output formats.
--
-- @since 0.1.0.0
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

-- | Lookup component by name
--
-- @since 0.1.0.0
lookupComponent :: Component.Name -> Either Component.Name Component
lookupComponent name =
    maybe (Left name) Right $ HashMap.lookup name allComponentsMap

------------------------------------------------------------------------------

-- | Run specified components
--
-- @since 0.1.0.0
run
  :: [Component]
  -> Handle        -- ^ output handle
  -> Maybe Handle  -- ^ optional debug handle
  -> OutputFormat
  -> IO Bool       -- ^ 'True' when upgrades are available
run components outHandle mDebugHandle outputFormat = do
    items <- fmap concat . forM components $ \component ->
      Component.run component mDebugHandle
    case outputFormat of
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
    return . not $ null items

------------------------------------------------------------------------------

-- | Run all components
--
-- @since 0.1.0.0
runAll
  :: Handle        -- ^ output handle
  -> Maybe Handle  -- ^ optional debug handle
  -> OutputFormat
  -> IO Bool       -- ^ 'True' when upgrades are available
runAll = run allComponents

------------------------------------------------------------------------------
-- $Internal

-- | Format a table with all columns left-justfied
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
    lens = map ((+) 2 . maximum . map T.length) $ transpose rows
