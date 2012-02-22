module Cryptd.Master.CLI (dispatch) where

import Control.Monad
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text

import Cryptd.Master.CLI.Options
import qualified Cryptd.Master.CLI.Partner as MPartner
import qualified Cryptd.Master.CLI.Arch as MArch
import qualified Cryptd.Master.CLI.Build as MBuild
import qualified Cryptd.Master.CLI.Serve as MServe

dispatchModes :: CLI -> IO ()
dispatchModes [] = error "Unable to parse arguments"
dispatchModes (("partner", CLICmd) : r) = MPartner.execute r
dispatchModes (("arch", CLICmd) : r) = MArch.execute r
dispatchModes (("build", CLICmd) : r) = MBuild.execute r
dispatchModes (("serve", CLICmd) : r) = MServe.execute r
dispatchModes (_ : r) = dispatchModes r

processHelp :: CLIData -> IO Bool
processHelp ("help", CLIHelp hmode hformat htext) = do
    putStr $ showText htext $ helpText [] hformat hmode
    return True
processHelp (_, _) =
    return False

dispatch :: IO ()
dispatch = do
    args <- processArgs allModes
    showed <- mapM processHelp args
    unless (True `elem` showed) $ dispatchModes (reverse args)
