{-
  Hapoid - A reviewer for your Bahasa Indonesia PO files

  Author:       Wisnu Adi Nurcahyo
  Contact:      <wisnu@nurcahyo.me>
  URL:          github.com/wisn/hapoid
-}

-- Libraries
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
            "check <path>", "   Check file in the specified path i.e \"check my/path\"",
            "", "               will detected as \"my/path/id.po\"",
            "about", "          Display about message",
            "version", "         Show Hapoid version"]

-- Options
options = ["--fuzzy", "        Review all fuzzy-translations"]

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

showSucc :: String -> String
showSucc msg = "[" ++ "\x1b[32m" ++ "INFO" ++ "\x1b[0m" ++ "] " ++ msg

showInfo :: String -> String
showInfo msg = "[" ++ "\x1b[34m" ++ "INFO" ++ "\x1b[0m" ++ "] " ++ msg

showWarn :: String -> String
showWarn msg = "[" ++ "\x1b[33m" ++ "WARN" ++ "\x1b[0m" ++ "] " ++ msg

showErr :: String -> String
showErr msg = "[" ++ "\x1b[31m" ++ "ERR" ++ "\x1b[0m" ++ "]  " ++ msg

simplify :: String -> String
simplify path = do
  if (length path) > 25
    then reverse $ (take 25 (reverse path)) ++ "..."
    else path

-- Checker
check :: String -> String -> IO ()
check path' opts' = do
  putStrLn $ name ++ " " ++ version
  putStrLn []

  loc  <- getCurrentDirectory

  let path  = if path' == "." then "id.po" else path'
      opts  = if opts' == "" then [] else words opts'
      file  = if path  == "id.po"
                then loc ++ "/" ++ path
                else path ++ "/id.po"

  exist <- doesFileExist file

  if exist
    then do
      putStrLn $ showInfo ("Checking " ++ simplify file)
      putStrLn []
    else
      putStrLn $ showErr ("File " ++ (simplify file) ++ " didn't exist!")

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
  | cmd == "help"    = display help
  | cmd == "about"   = display about
  | cmd == "check"   = check path opts
  | cmd == "version" = display ["", name ++ " " ++ version, ""]
  | otherwise = display ["",
                         showErr "Unknown command!",
                         showInfo "Please check available commands with \"hapoid help\"!",
                         ""]

run :: [String] -> IO ()
run args = do
  let str   = words $ unwords $ parse args
      cmd   = head str
      path  = str !! 1
      opts  = last str

  if ((cmd == "help" || cmd == "about" || cmd == "version")
    && ((isOption opts) || path /= "."))
      then do
        putStrLn $ showWarn "Given useless argument(s) in this command."
        execute cmd path opts
      else execute cmd path opts

-- Main
main = do
  args <- getArgs

  if (null args)
    then display help
    else run args
