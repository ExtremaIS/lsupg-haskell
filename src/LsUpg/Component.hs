------------------------------------------------------------------------------
-- |
-- Module      : LsUpg.Component
-- Description : component types
-- Copyright   : Copyright (c) 2021-2024 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

#if !MIN_VERSION_text(1,2,4)
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

module LsUpg.Component
  ( -- * Types
    Component(..)
  , Options(..)
  , Item(..)
  , Name
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

-- https://hackage.haskell.org/package/hashable
import Data.Hashable (Hashable)

-- https://hackage.haskell.org/package/template-haskell
#if !MIN_VERSION_text(1,2,4)
import qualified Language.Haskell.TH as TH
#endif
import qualified Language.Haskell.TH.Syntax as THS

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- https://hackage.haskell.org/package/unordered-containers
import qualified Data.HashMap.Strict as HashMap

-- https://hackage.haskell.org/package/vector
import qualified Data.Vector as V

------------------------------------------------------------------------------
-- $Types

-- | Component API
--
-- @since 0.3.0.0
data Component
  = Component
    { name        :: !Name
    , description :: !String
    , run         :: Options -> IO [Item]
    }

------------------------------------------------------------------------------

-- | Component options
--
-- @since 0.3.0.0
data Options
  = Options
    { mDebugHandle :: !(Maybe Handle)
    , mNixPath     :: !(Maybe FilePath)
    }

------------------------------------------------------------------------------

-- | Upgrade item
--
-- @since 0.1.0.0
data Item
  = Item
    { componentName    :: !Name
    , itemName         :: !Text
    , installedVersion :: !(Maybe Text)
    , availableVersion :: !(Maybe Text)
    }
  deriving (Eq, Show)

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
    , ("installed_version", maybe "" TTC.convert installedVersion)
    , ("available_version", maybe "" TTC.convert availableVersion)
    ]

instance CSV.ToRecord Item where
  toRecord Item{..} = V.fromList
    [ TTC.render componentName
    , TTC.convert itemName
    , maybe "" TTC.convert installedVersion
    , maybe "" TTC.convert availableVersion
    ]

------------------------------------------------------------------------------

-- | Component name
--
-- @since 0.1.0.0
newtype Name = Name { unName :: Text }
  deriving (Show, THS.Lift)
  deriving newtype (Eq, Hashable)

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

#if !MIN_VERSION_text(1,2,4)
instance THS.Lift Text where
  lift = TH.appE (TH.varE 'T.pack) . TH.stringE . T.unpack
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped =
    fmap THS.TExp . TH.appE (TH.varE 'T.pack) . TH.stringE . T.unpack
#endif
#endif
