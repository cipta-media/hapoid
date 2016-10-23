{-
  Hapol - A 
-}

import System.IO
import System.Exit
import System.Environment
import Control.Monad

-- Version
version          = "0.0.1-alpha"
versionMajor     = 0
versionMinor     = 0
versionRelease   = 1
versionAttribute = "alpha"

-- Release Date
released = ""

-- Information
about = ["Hapol " ++ version ++ " by Wisnu Adi Nurcahyo",
         "Released at <not released yet>"]

information = ["Usage: hapol COMMAND [OPTIONS] [ARGS]...",
               "",
               "Commands:",
               "  about                     Show Hapol about message",
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

moreInformation = "Type \"hapol help\" for more information!"

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