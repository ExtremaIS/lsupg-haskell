------------------------------------------------------------------------------
-- |
-- Module      : Build
-- Description : build information
-- Copyright   : Copyright (c) 2021-2024 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Build
  ( version
  ) where

-- https://hackage.haskell.org/package/base
import Control.Monad ((<=<))
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Version
import System.IO.Error (tryIOError)
import qualified System.Info

-- http://hackage.haskell.org/package/template-haskell
import Language.Haskell.TH (ExpQ, runIO, stringE)

-- (lsupg)
import qualified LsUpg

------------------------------------------------------------------------------

-- | Compiler version string (internal)
--
-- The full compiler version is used when it is available.
compilerVersion :: String
compilerVersion = Data.Version.showVersion
#if MIN_VERSION_base (4,15,0)
    System.Info.fullCompilerVersion
#else
    System.Info.compilerVersion
#endif

------------------------------------------------------------------------------

-- | Version string with build information when built on Alpine
--
-- @since 0.4.0.0
version :: ExpQ
version = stringE <=< runIO $ do
    mAlpineVersion <-
      either (const Nothing) (Just . List.dropWhileEnd Char.isSpace) <$>
        tryIOError (readFile "/etc/alpine-release")
    pure $ case mAlpineVersion of
      Just alpineVersion -> unwords
        [ LsUpg.version
        , "built on Alpine"
        , alpineVersion
        , "using"
        , System.Info.compilerName
        , compilerVersion
        ]
      Nothing -> LsUpg.version
