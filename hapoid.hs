{-
  Hapoid - A reviewer for your Bahasa Indonesia PO files

  Author:       Wisnu Adi Nurcahyo
  Contact:      <wisnu@nurcahyo.me>
  URL:          github.com/wisn/hapoid
-}

import System.IO
import System.Exit
import System.Directory
import System.Environment
import Control.Monad

-- Version
version          = "0.0.1-alpha"
versionMajor     = "0"
versionMinor     = "0"
versionRelease   = "1"
versionAttribute = "alpha"
-- Release
released = "<not released yet>" -- > Not released yet
-- About
name   = "Hapoid"
author = "Wisnu Adi Nurcahyo"

-- Commands
commands = ["help",  "           Display this message",
            "check", "          Check id.po file in the current directory",
            "check <path>", "   Check file in the specified path",
            "about", "          Display about message"]

-- Options
options = ["--fuzzy", "    Review all fuzzy-translations"]

-- Modules
display :: [String] -> IO ()
display text = putStr $ unlines text

extract :: [String] -> [String]
extract [] = []
extract (cmd:inf:next) = [cmd ++ inf] ++ (extract next)

getCmd :: String -> String
getCmd input
  | isOption input = ""
  | otherwise      = input

isOption :: String -> Bool
isOption arg
  | (arg !! 0) == '-' && (arg !! 1) == '-' = True
  | otherwise = False

getOption :: [String] -> String
getOption []     = []
getOption (x:xs)
  | isOption x = x ++ " " ++ (getOption xs)
  | otherwise  = getOption xs

getPath :: String -> String
getPath str
  | not (isOption str) = str
  | otherwise          = "."

parse :: [String] -> [String]
parse (arg:args) = do
  let cmd   = getCmd arg
      path' = if args == [] then ["."] else args
      path  = getPath (head path')
      opts  = getOption args

      result = cmd ++ " " ++ path ++ " " ++ opts

  return result

check :: String -> String -> IO ()
check path' opts' = do
  putStrLn $ name ++ " " ++ version
  putStrLn []

  loc  <- getCurrentDirectory

  let path = if path' == "." then "id.po" else path'
      opts = if opts' == "" then [] else words opts'
      file  = if path == "id.po"
                then loc ++ "/" ++ path
                else path

  exist <- doesFileExist file

  if exist
    then do
      putStrLn $ "[ ! ] Checking " ++ file ++ " ..."
      putStrLn []
    else
      putStrLn $ "[!!!] File " ++ file ++ " didn't exist!"

  putStrLn []

-- Messages
about = ["",
         name ++ " " ++ version ++ " by " ++ author,
         "Released at " ++ released,
         ""]
help  = ["Usage: hapoid COMMAND [PATH] [OPTION] [ARGS]...",
         "",
         "Commands",
         unlines $ extract commands,
         "",
         "Options",
         unlines $ extract options,
         ""]

-- Executor
execute :: String -> String -> String -> IO ()
execute cmd path opts
  | cmd == "help"  = display help
  | cmd == "about" = display about
  | cmd == "check" = check path opts
  | otherwise = display ["",
                         "[!!!] Unknown command!",
                         "[ ! ] Please check available commands with \"hapoid help\"!",
                         ""]

run :: [String] -> IO ()
run args = do
  let str   = words $ unwords $ parse args
      cmd   = head str
      path  = str !! 1
      opts  = last str

  execute cmd path opts

-- Main
main = do
  args <- getArgs

  if (null args)
    then display help
    else run args

{-
-- Version
version          = "0.0.1-alpha"
versionMajor     = 0
versionMinor     = 0
versionRelease   = 1
versionAttribute = "alpha"

-- Release Date
released = ""

-- Information
about = ["Hapoid " ++ version ++ " by Wisnu Adi Nurcahyo",
         "Released at <not released yet>"]

information = ["Usage: hapoid COMMAND [OPTIONS] [ARGS]...",
               "",
               "Commands:",
               "  about                     Show Hapoid about message",
               "  help                      Show this message and exit",
               "  help <topic>              Show help message for the selected topic",
               "  check <pofile>            Finding errors on .po file",
               "  check <pofile> <options>  Finding errors on .po file with an option",
               "",
               "Options:",
               "  --fuzzy                   Treat all fuzzy translation as an error"
               ]

displayInformation = do
  putStr $ unlines information
  exitSuccess

moreInformation = "Type \"hapoid help\" for more information!"

errorIn :: String -> String -> String
errorIn typ msg' = do
  let msg = show msg'

  case typ of []    -> unlines ["Error: No such command " ++ msg ++ "!", "", moreInformation]
              "cmd" -> unlines ["Error: " ++ msg ++ " command need one more argument!", "", moreInformation]
              "opt" -> unlines ["Error: Option executed along with a command!", "", moreInformation]
              _     -> unlines ["Error: Unknown error appear!", "", moreInformation]

-- Command Executor
singleCmd :: [String] -> String
singleCmd (arg:_) = do
  case arg of "help"    -> unlines information
              "about"   -> unlines about
              "check"   -> errorIn "cmd" "check"
              "--fuzzy" -> errorIn "opt" []
              _         -> errorIn [] arg

-- Command Parser
command = ["help", "check", "about"]

commander :: [String] -> IO ()
commander args = do
  if length args == 1
    then putStr $ singleCmd args
    else do
      if elem (head args) command
        then putStrLn "Execute command!"
        else putStr $ errorIn [] (head args)

-- Main Program
main :: IO ()
main = do
  args <- getArgs

  when (null args)
    displayInformation

  print args
  commander args

-}
