{-
  Hapoid - A reviewer for your Bahasa Indonesia PO files

  Author:       Wisnu Adi Nurcahyo
  Contact:      <wisnu@nurcahyo.me>
  URL:          github.com/wisn/hapoid
-}

-- [Libraries]
import System.Directory (getCurrentDirectory, doesFileExist)
import System.Environment (getArgs)
import Data.Char (toLower)
import Data.List (isInfixOf)

-- [Version]
version          = "0.0.1-alpha"
versionMajor     = "0"
versionMinor     = "0"
versionRelease   = "1"
versionAttribute = "alpha"

-- [Release]
released = "<not released yet>" -- > Not released yet

-- [About]
name   = "Hapoid"
author = "Wisnu Adi Nurcahyo"

-- [Commands]
commands = ["help",  "           Display this message",
            "check", "          Check id.po file in the current directory",
            "check <path>", "   Check file in the specified path i.e \"check my/path\"",
            "", "               will detected as \"my/path/id.po\"",
            "about", "          Display about message",
            "version", "         Show Hapoid version"]

-- [Options]
options = ["--fuzzy", "        Review all fuzzy-translations"]

-- [Modules]
-- Display a message
display :: [String] -> IO ()
display text = putStr $ unlines text

-- Extract an information like `commands` or `options`
extract :: [String] -> [String]
extract [] = []
extract (cmd:inf:next) = [cmd ++ inf] ++ (extract next)

-- Detect and get a command
getCmd :: String -> String
getCmd input
  | isOption input = ""
  | otherwise      = input

-- Check, it's a command?
isOption :: String -> Bool
isOption arg
  | (arg !! 0) == '-' && (arg !! 1) == '-' = True
  | otherwise = False

-- Detect and get an option
getOption :: [String] -> String
getOption []     = []
getOption (x:xs)
  | isOption x = x ++ " " ++ (getOption xs)
  | otherwise  = getOption xs

-- Detect and get a path
getPath :: String -> String
getPath str
  | not (isOption str) = str
  | otherwise          = "."

-- Parse arguments into separated list `cmd` `path` `opts`
parse :: [String] -> [String]
parse (arg:args) = do
  let cmd   = getCmd arg
      path' = if args == [] then ["."] else args
      path  = getPath (head path')
      opts  = getOption args

      result = cmd ++ " " ++ path ++ " " ++ opts

  return result

-- Makes text green
makesGreen :: String -> String
makesGreen text = "\x1b[32m" ++ text ++ "\x1b[0m"

-- Makes text blue
makesBlue :: String -> String
makesBlue text = "\x1b[34m" ++ text ++ "\x1b[0m"

-- Makes text yellow
makesYellow :: String -> String
makesYellow text = "\x1b[33m" ++ text ++ "\x1b[0m"

-- Makes text red
makesRed :: String -> String
makesRed text = "\x1b[31m" ++ text ++ "\x1b[0m"

-- Display a success message
showSucc :: String -> String
showSucc msg = "[" ++ (makesGreen "SUCC") ++ "] " ++ msg

-- Display an information message
showInfo :: String -> String
showInfo msg = "[" ++ (makesBlue "INFO") ++ "] " ++ msg

-- Display a warning message
showWarn :: String -> String
showWarn msg = "[" ++ (makesYellow "WARN") ++ "] " ++ msg

-- Display an error message
showErr :: String -> String
showErr msg = "[" ++ (makesRed "ERR") ++ "]  " ++ msg

-- Check and truncate path information
simplify :: String -> String
simplify path = do
  if (length path) > 40
    then reverse $ (take 40 (reverse path)) ++ "..."
    else path

-- Check if it's a msgstr
isMsgstr :: String -> Bool
isMsgstr text
  | text == ""                      = False
  | (head $ words text) == "msgstr" = True
  | otherwise                       = False

-- Get the msgstr
getMsgstr :: String -> String
getMsgstr text' = do
  let text    = words text'
      content = unwords $ tail text

  if (head text) == ""
    then ""
    else content

-- Give suggestion
reviewSuggestion :: String -> String -> String
reviewSuggestion sugg from = "[" ++ (makesBlue "SUGG") ++ "] You may use " ++ (makesBlue sugg) ++ " instead of " ++ (makesRed from)

-- [Reviewer]
-- Review the content
fixWords :: String -> String
fixWords text
  | isInfixOf "font" text        = reviewSuggestion "huruf" "font"
  | isInfixOf "update" text      = reviewSuggestion "pembaruan" "update"
  | isInfixOf "diinstalasi" text = reviewSuggestion "dipasang" "diinstalasi"
  | otherwise                    = ""

recursiveCheck :: [String] -> [String]
recursiveCheck [] = []
recursiveCheck (x:xs) = do
  let check = fixWords (map toLower x)

  if check /= ""
    then check : (recursiveCheck xs)
    else recursiveCheck xs

review :: [String] -> Integer -> IO ()
review [] _ = do
  putStrLn []
  putStrLn $ showSucc "Checking done!"
review (x:xs) line = do
  let f = if x == "" then "" else head (words x)

  if isMsgstr f
    then do
      let msgstr = getMsgstr x
          review = recursiveCheck (words msgstr)

      if (length review) > 0
        then do
          putStrLn $ "[Line: " ++ (show line) ++ "]"
          putStrLn $ unlines review
        else return ()
    else return ()

  review xs (line + 1)

-- Check both given path and option(s)
check :: String -> String -> IO ()
check path'' opts' = do
  putStrLn $ name ++ " " ++ version
  putStrLn []

  loc  <- getCurrentDirectory

  let path'  = if path'' == "." then "id.po" else path''
      opts  = if opts' == "" then [] else words opts'
      path  = if path'  == "id.po"
                then loc ++ "/" ++ path'
                else path' ++ "/id.po"

  exist <- doesFileExist path

  if exist
    then do
      file <- readFile path

      let content = lines file

      putStrLn $ showInfo ("Checking " ++ simplify path)
      putStrLn []

      review content 1
    else
      putStrLn $ showErr ("File " ++ (simplify path) ++ makesRed " didn't exist!")

  putStrLn []

-- [Messages]
-- About message
about = ["",
         name ++ " " ++ version ++ " by " ++ author,
         "Released at " ++ released,
         ""]

-- Help message
help  = ["Usage: hapoid COMMAND [PATH] [OPTION] [ARGS]...",
         "",
         "Commands",
         unlines $ extract commands,
         "",
         "Options",
         unlines $ extract options,
         ""]

-- [Executor]
-- Execute given arguments
execute :: String -> String -> String -> IO ()
execute cmd path opts
  | cmd == "help"    = display help
  | cmd == "about"   = display about
  | cmd == "check"   = check path opts
  | cmd == "version" = display [name ++ " " ++ version]
  | otherwise = display ["",
                         showErr "Unknown command!",
                         showInfo "Please check available commands with " ++ (makesBlue "hapoid help") ++ "!",
                         ""]

-- Run the program
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

-- [Main]
main = do
  args <- getArgs

  if (null args)
    then display help
    else run args
