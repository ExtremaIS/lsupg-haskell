{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

-- https://hackage.haskell.org/package/ansi-wl-pprint
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import Text.PrettyPrint.ANSI.Leijen (Doc)

-- https://hackage.haskell.org/package/base
import Control.Applicative (many, optional)
import Control.Monad (when)
import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import System.IO (hPutStrLn, stderr, stdout)

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- https://hackage.haskell.org/package/typed-process
import qualified System.Process.Typed as TP

-- (lsupg)
import qualified LsUpg
import qualified LsUpg.Component as Component
import LsUpg.Component.Nix (defaultNixPath)

-- (lsupg:executable)
import qualified LibOA

------------------------------------------------------------------------------
-- $Defaults

defaultFormat :: LsUpg.OutputFormat
defaultFormat = LsUpg.OutputHuman

------------------------------------------------------------------------------
-- $Options

data Options
  = Options
    { debugOpt      :: !Bool
    , formatOpt     :: !LsUpg.OutputFormat
    , dockerOpt     :: !(Maybe String)
    , nixPathOpt    :: !(Maybe FilePath)
    , componentArgs :: ![Component.Name]
    }

debugOption :: OA.Parser Bool
debugOption = OA.switch $ mconcat
    [ OA.long "debug"
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

dockerOption :: OA.Parser String
dockerOption = OA.strOption $ mconcat
    [ OA.long "docker"
    , OA.metavar "IMAGE"
    , OA.help "Docker image"
    ]

nixPathOption :: OA.Parser FilePath
nixPathOption = OA.strOption $ mconcat
    [ OA.long "nix-path"
    , OA.metavar "PATH"
    , OA.help $
        "check Nix upgrades against PATH (default: " ++ defaultNixPath ++ ")"
    ]

componentArguments :: OA.Parser [Component.Name]
componentArguments = many . OA.argument (OA.eitherReader TTC.parse) $ mconcat
    [ OA.metavar "COMPONENT ..."
    , OA.help "components (default: all)"
    ]

options :: OA.Parser Options
options = Options
    <$> debugOption
    <*> formatOption
    <*> optional dockerOption
    <*> optional nixPathOption
    <*> componentArguments

------------------------------------------------------------------------------
-- $RunFunctions

runDocker :: String -> Options -> IO a
runDocker image Options{..} = do
    exePath <- getExecutablePath
    let args = concat $ catMaybes
          [ Just
              [ "run", "--rm", "-it", "-u", "root"
              , "-v", exePath ++ ":/usr/local/bin/lsupg:ro"
              , image
              , "/usr/local/bin/lsupg"
              ]
          , if debugOpt then Just ["--debug"] else Nothing
          , (\nixPath -> ["--nix-path", nixPath]) <$> nixPathOpt
          , if formatOpt == defaultFormat
              then Nothing
              else Just ["--format", TTC.render formatOpt]
          , Just (map TTC.render componentArgs)
          ]
    when debugOpt . hPutStrLn stderr . unwords $ "[lsupg]" : "docker" : args
    exitWith =<< TP.runProcess (TP.proc "docker" args)

------------------------------------------------------------------------------

runAll :: Options -> IO a
runAll Options{..} = do
    let mDebugHandle = bool Nothing (Just stderr) debugOpt
    hasUpgrades <- LsUpg.runAll stdout mDebugHandle nixPathOpt formatOpt
    exitWith $ bool ExitSuccess (ExitFailure 3) hasUpgrades

------------------------------------------------------------------------------

runSpecified :: Options -> IO a
runSpecified Options{..} =
    case partitionEithers (map LsUpg.lookupComponent componentArgs) of
      ([], components) -> do
        let mDebugHandle = bool Nothing (Just stderr) debugOpt
        hasUpgrades <-
          LsUpg.run components stdout mDebugHandle nixPathOpt formatOpt
        exitWith $ bool ExitSuccess (ExitFailure 3) hasUpgrades
      (names, _components) -> do
        hPutStrLn stderr $ "error: component(s) not found: " ++
          intercalate ", " (map TTC.render names)
        exitWith $ ExitFailure 2

------------------------------------------------------------------------------
-- $Main

main :: IO ()
main = do
    opts <- OA.execParser pinfo
    case (dockerOpt opts, null (componentArgs opts)) of
      (Just image, _isEmpty) -> runDocker image opts
      (Nothing,    True)     -> runAll opts
      (Nothing,    False)    -> runSpecified opts
  where
    pinfo :: OA.ParserInfo Options
    pinfo
      = OA.info (LibOA.helper <*> LibOA.versioner LsUpg.version <*> options)
      $ mconcat
          [ OA.fullDesc
          , OA.progDesc "list items that can be upgraded"
          , OA.failureCode 2
          , OA.footerDoc . Just $ LibOA.vspace
              [ outputFormatHelp
              , componentsHelp
              , exitCodesHelp
              ]
          ]

    outputFormatHelp :: Doc
    outputFormatHelp
      = LibOA.section "FORMATs:"
      . Doc.text
      . intercalate ", "
      . map (TTC.render @LsUpg.OutputFormat)
      $ [minBound ..]

    componentsHelp :: Doc
    componentsHelp = LibOA.section "COMPONENTs:" $ LibOA.table_ 2
      [ [TTC.render (Component.name c), Component.description c]
      | c <- LsUpg.allComponents
      ]

    exitCodesHelp :: Doc
    exitCodesHelp = LibOA.section "Exit codes:" $ LibOA.table_ 2
      [ ["0", "no upgrades available"]
      , ["1", "program error"]
      , ["2", "program usage error"]
      , ["3", "one or more upgrades available"]
      ]
