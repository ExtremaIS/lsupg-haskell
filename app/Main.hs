{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

-- https://hackage.haskell.org/package/ansi-wl-pprint
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import Text.PrettyPrint.ANSI.Leijen (Doc)

-- https://hackage.haskell.org/package/base
import Control.Applicative (optional)
import Control.Monad (when)
import Data.Bool (bool)
import Data.List (intercalate)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr, stdout)
import Text.Read (readMaybe)

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- https://hackage.haskell.org/package/typed-process
import qualified System.Process.Typed as TP

-- (lsupg)
import qualified LsUpg
import qualified LsUpg.Component as Component

-- (lsupg:executable)
import qualified LibOA

------------------------------------------------------------------------------
-- $Defaults

defaultFormat :: LsUpg.OutputFormat
defaultFormat = LsUpg.OutputHuman

defaultUpdate :: Int
defaultUpdate = 8

------------------------------------------------------------------------------
-- $Options

data Options
  = Options
    { componentOpt :: !(Maybe Component.Name)
    , debugOpt     :: !Bool
    , formatOpt    :: !LsUpg.OutputFormat
    , imageOpt     :: !(Maybe String)
    , updateOpt    :: !Int
    }

componentOption :: OA.Parser Component.Name
componentOption = OA.option (OA.eitherReader TTC.parse) $ mconcat
    [ OA.long "component"
    , OA.short 'c'
    , OA.metavar "COMPONENT"
    , OA.help "component (default: all)"
    ]

debugOption :: OA.Parser Bool
debugOption = OA.switch $ mconcat
    [ OA.long "debug"
    , OA.short 'd'
    , OA.help "show debug output"
    ]

formatOption :: OA.Parser LsUpg.OutputFormat
formatOption = OA.option (OA.eitherReader TTC.parse) $ mconcat
    [ OA.long "format"
    , OA.short 'f'
    , OA.metavar "FORMAT"
    , OA.value defaultFormat
    , OA.showDefaultWith TTC.render
    , OA.help "output format"
    ]

imageOption :: OA.Parser String
imageOption = OA.strOption $ mconcat
    [ OA.long "image"
    , OA.short 'i'
    , OA.metavar "IMAGE"
    , OA.help "Docker image"
    ]

updateOption :: OA.Parser Int
updateOption = OA.option (OA.eitherReader parse) $ mconcat
    [ OA.long "update"
    , OA.short 'u'
    , OA.metavar "HOURS"
    , OA.value defaultUpdate
    , OA.showDefault
    , OA.help "update if not updated within HOURS"
    ]
  where
    parse :: String -> Either String Int
    parse = maybe (Left "invalid HOURS") Right . readMaybe

options :: OA.Parser Options
options = Options
    <$> optional componentOption
    <*> debugOption
    <*> formatOption
    <*> optional imageOption
    <*> updateOption

------------------------------------------------------------------------------
-- $Main

main :: IO ()
main = do
    Options{..} <- OA.execParser pinfo
    case imageOpt of
      Just image -> do
        exePath <- getExecutablePath
        let args =
              [ "run", "--rm", "-it", "-u", "root"
              , "-v", exePath ++ ":/usr/local/bin/lsupg:ro"
              , image
              , "/usr/local/bin/lsupg"
              , "--format", TTC.render formatOpt
              , "--update", show updateOpt
              ] ++ bool [] ["--debug"] debugOpt ++
              case componentOpt of
                Just component -> ["--component", TTC.render component]
                Nothing        -> []
        when debugOpt . hPutStrLn stderr . unwords $
          "[lsupg]" : "docker" : args
        exitWith =<< TP.runProcess (TP.proc "docker" args)
      Nothing -> case componentOpt of
        Just name -> do
          mUpdate <- LsUpg.runComponent name
            stdout (bool Nothing (Just stderr) debugOpt)
            (Just . fromIntegral $ updateOpt * 3600) formatOpt
          case mUpdate of
            Just True  -> exitWith $ ExitFailure 3
            Just False -> exitSuccess
            Nothing -> do
              hPutStrLn stderr "error: component not found"
              exitWith $ ExitFailure 2
        Nothing -> do
          update <- LsUpg.runAllComponents
            stdout (bool Nothing (Just stderr) debugOpt)
            (Just . fromIntegral $ updateOpt * 3600) formatOpt
          exitWith $ bool ExitSuccess (ExitFailure 3) update
  where
    pinfo :: OA.ParserInfo Options
    pinfo
      = OA.info (LibOA.helper <*> LibOA.versioner LsUpg.version <*> options)
      $ mconcat
          [ OA.fullDesc
          , OA.progDesc "list items that can be upgraded"
          , OA.failureCode 2
          , OA.footerDoc . Just $ LibOA.vspace
              [ componentsHelp
              , outputFormatHelp
              , exitCodesHelp
              ]
          ]

    componentsHelp :: Doc
    componentsHelp
      = LibOA.section "COMPONENT options:"
      . Doc.text
      . intercalate ", "
      $ map (TTC.render . Component.name) LsUpg.allComponents

    outputFormatHelp :: Doc
    outputFormatHelp
      = LibOA.section "FORMAT options:"
      . Doc.text
      . intercalate ", "
      . map (TTC.render @LsUpg.OutputFormat)
      $ [minBound ..]

    exitCodesHelp :: Doc
    exitCodesHelp = LibOA.section "Exit codes:" $ LibOA.table
      [ ("0", "no upgrades available")
      , ("1", "program error")
      , ("2", "program usage error")
      , ("3", "one or more upgrades available")
      ]
