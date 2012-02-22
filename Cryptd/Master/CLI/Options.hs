{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cryptd.Master.CLI.Options where

import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text

type CLIData = (String, CLIValue String)
type CLI = [CLIData]

data CLIValue a
    = CLIValue a
    | CLICmd
    | CLIHelp
        { helpMode :: Mode CLI
        , helpFormat :: HelpFormat
        , helpTextFormat :: TextFormat
        }
    | CLIBool
    deriving (Show, Eq)

instance Eq a => Eq (Mode a) where
    m1 == m2 = modeValue m1 == modeValue m2

update :: String -> String -> CLI -> Either String CLI
update name value acc =
    Right $ (name, CLIValue value) : acc

updateCmd :: String -> CLI -> Either String CLI
updateCmd name acc =
    Right $ (name, CLICmd) : acc

updateBool :: String -> Bool -> CLI -> CLI
updateBool name True  acc = (name, CLIBool) : acc
updateBool _    False acc = acc

updateHelp :: Mode CLI -> HelpFormat -> TextFormat -> CLI -> CLI
updateHelp climode hformat tformat acc =
    ("help", CLIHelp climode hformat tformat) : acc

flagArgReq :: Update a -> FlagHelp -> Arg a
flagArgReq upd help = wrap $ flagArg upd help
    where wrap f = f{argRequire = True}

addSubModes :: Mode CLI -> [Mode CLI] -> Mode CLI
addSubModes parent children =
    parent { modeGroupModes = toGroup altchildren }
  where
    parval = modeValue parent
    updater m@Mode {modeValue = val} =
        m { modeValue = val ++ parval }
    altchildren = map updater children

genericFlags :: Mode CLI -> [Flag CLI]
genericFlags m =
    [flagHelpFormat (updateHelp m)]

addGeneric :: Mode CLI -> Mode CLI
addGeneric oldmode@Mode{modeGroupFlags = oldgroup} =
    oldmode { modeGroupFlags = newgroup }
  where oldhidden = groupHidden oldgroup
        newhidden = oldhidden ++ genericFlags oldmode
        newgroup = oldgroup { groupHidden = newhidden }

injectGeneric :: Mode CLI -> Mode CLI
injectGeneric m | null $ modeModes m = addGeneric m
                | otherwise =
                    addGeneric $ m{modeGroupModes = toGroup newmodes}
                  where
                    newmodes = map injectGeneric (modeModes m)

argsCmd :: String -> String -> [Arg CLI] -> [Flag CLI] -> Mode CLI
argsCmd name help args flags =
    newmode
  where
    newmode = (modeEmpty [(name, CLICmd)])
        { modeNames = [name]
        , modeHelp = help
        , modeArgs = (args, Nothing)
        , modeGroupFlags = toGroup flags
        }

data Container = Container
    { cIdFlag :: Arg CLI
    , cContentFlags :: [Arg CLI]
    , cListHelp :: String
    , cAddHelp :: String
    , cChangeHelp :: String
    , cDeleteHelp :: String
    }

makeContainerMode :: String -> String -> Container -> Mode CLI
makeContainerMode name help spec =
    addSubModes zipped submodes
  where
    zipped = argsCmd name help [] []
    submodes = [modeList, modeAdd, modeChange, modeDelete]

    modeList = argsCmd "list" (cListHelp spec) [] []
    modeAdd = argsCmd "add" (cAddHelp spec) addChangeArgs []
    modeChange = argsCmd "change" (cChangeHelp spec) addChangeArgs []
    modeDelete = argsCmd "delete" (cDeleteHelp spec) deleteArgs []

    addChangeArgs = cIdFlag spec : cContentFlags spec
    deleteArgs = [cIdFlag spec]

partner :: Mode CLI
partner =
    makeContainerMode prog help spec
  where
    prog = "partner"
    help = "Change, list, delete or add partner IDs."
    spec = Container
        { cIdFlag = flagArgReq (update "partnerid") "PARTNERID"
        , cContentFlags = [flagArgReq (update "partnerurl") "URL"]
        , cListHelp = "List partners"
        , cAddHelp = "Add a partner"
        , cChangeHelp = "Change partner URL"
        , cDeleteHelp = "Delete a partner"
        }

arch :: Mode CLI
arch =
    makeContainerMode prog help spec
  where
    prog = "arch"
    help = "Change, list, delete or add architectures."
    spec = Container
        { cIdFlag = flagArgReq (update "archname") "ARCHNAME"
        , cContentFlags = [flagArg (update "binary") "BINARY"]
        , cListHelp = "List architectures"
        , cAddHelp = "Add an architecture"
        , cChangeHelp = "Replace binary for an architecture"
        , cDeleteHelp = "Delete an architecture"
        }

build :: Mode CLI
build = argsCmd "build" "Build a slave binary." args []
    where args = [ flagArgReq (update "partnerid") "PARTNERID"
                 , flagArgReq (update "archname") "ARCHNAME"
                 , flagArg (update "binary") "BINARY"
                 ]

serve :: Mode CLI
serve = argsCmd "serve" "Run the master server." [] flags
    where flags = [ flagReq ["l", "listenaddr"] (update "listenaddr")
                            "IP/HOST" "Address to bind to (Default: 127.0.0.1)"
                  , flagReq ["p", "port"] (update "port")
                            "PORT" "Port to bind to (Default: 16661)"
                  , flagReq ["L", "tls-addr"] (update "tlsaddr")
                            "IP/HOST" "Address to bind TLS socket to (Default: 0.0.0.0)"
                  , flagReq ["P", "tls-port"] (update "tlsport")
                            "PORT" "Port to bind TLS socket to (Default: 26662)"
                  , flagReq ["u", "url"] (update "url")
                            "URL" "API backend URL (Default: http://localhost/api/)"
                  , flagBool ["f", "foreground"] (updateBool "foreground")
                            "Run master server in the foreground"
                  ]

cryptd :: Mode CLI
cryptd =
    newmode { modeGroupFlags = newgroup }
  where
    newmode = addSubModes zipped submodes
    group = modeGroupFlags newmode
    newgroup = group { groupNamed = [("Generic Flags", genericFlags newmode)] }
    zipped = (argsCmd prog help [] []) { modeHelpSuffix = summary }
    submodes = map injectGeneric [partner, arch, build, serve]
    prog = "cryptd"
    help = "Run cryptd SUBCOMMAND --help to get more info."
    summary = [ "MoonID encryption daemon - Master server"
              , "(C) 2012 RedMoon Studios GmbH & Co KG"
              ]

allModes :: Mode CLI
allModes = cryptd
