{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LsUpg.Component
  ( -- * Constants
    csvNull
    -- * Types
  , Component(..)
  , Item(..)
  , Name
  , Status(..)
  ) where

-- https://hackage.haskell.org/package/aeson
import qualified Data.Aeson as A
import Data.Aeson (ToJSON, (.=))

-- https://hackage.haskell.org/package/base
import Control.Monad (unless, when)
import Data.Bifunctor (first)
import Data.Char (isPrint, isSpace)
import Data.Maybe (catMaybes)
import Data.String (IsString(fromString))
import System.IO (Handle)

-- https://hackage.haskell.org/package/cassava
import qualified Data.Csv as CSV

-- https://hackage.haskell.org/package/template-haskell
import qualified Language.Haskell.TH.Syntax as THS

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

-- https://hackage.haskell.org/package/time
import Data.Time.Clock (UTCTime)

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- https://hackage.haskell.org/package/unordered-containers
import qualified Data.HashMap.Strict as HashMap

-- https://hackage.haskell.org/package/vector
import qualified Data.Vector as V

------------------------------------------------------------------------------
-- $Constants

csvNull :: CSV.Field
csvNull = ""

------------------------------------------------------------------------------
-- $Types

data Component
  = Component
    { name      :: !Name
    , getStatus :: Maybe Handle -> IO Status
    , doUpdate  :: Maybe Handle -> IO ()
    , getItems  :: Maybe Handle -> IO [Item]
    }

------------------------------------------------------------------------------

data Item
  = Item
    { componentName    :: !Name
    , itemName         :: !Text
    , installedVersion :: !(Maybe Text)
    , availableVersion :: !(Maybe Text)
    }
  deriving Show

instance ToJSON Item where
  toJSON Item{..} = A.object $ catMaybes
    [ Just $ "component_name" .= componentName
    , Just $ "item_name" .= itemName
    , ("installed_version" .=) <$> installedVersion
    , ("available_version" .=) <$> availableVersion
    ]

instance CSV.ToNamedRecord Item where
  toNamedRecord Item{..} = HashMap.fromList
    [ ("component_name", TTC.render componentName)
    , ("item_name", TTC.convert itemName)
    , ("installed_version", maybe csvNull TTC.convert installedVersion)
    , ("available_version", maybe csvNull TTC.convert availableVersion)
    ]

instance CSV.ToRecord Item where
  toRecord Item{..} = V.fromList
    [ TTC.render componentName
    , TTC.convert itemName
    , maybe csvNull TTC.convert installedVersion
    , maybe csvNull TTC.convert availableVersion
    ]

------------------------------------------------------------------------------

newtype Name = Name { unName :: Text }
  deriving (Show, THS.Lift)
  deriving newtype (Eq)

instance IsString Name where
  fromString = TTC.parseUnsafe

instance TTC.Parse Name where
  parse = TTC.asT $ \t -> first TTC.fromS $ do
    when (T.null t) $ Left "component name is empty"
    unless (T.all isPrint t) $ Left "component name has invalid character(s)"
    when (T.any isSpace t) $ Left "component name contains space character(s)"
    pure $ Name t

instance TTC.Render Name where
  render = TTC.convert . unName

instance CSV.ToField Name where
  toField = TTC.render

instance ToJSON Name where
  toJSON = A.String . TTC.render

------------------------------------------------------------------------------

data Status
  = NotFound
  | Empty
  | Updated !UTCTime
  deriving Show
