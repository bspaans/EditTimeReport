module Main where


import Query
import System 
import System.Console.GetOpt
import System.IO
import Control.Monad
import Control.Arrow
import System.FilePath
import System.Directory
import Data.Maybe

import Stats

data Options = Options {
                         interactive :: Bool
                       , askOptions  :: Bool
                       , ext         :: Extensions          
                       , output      :: [String]           -- not doing anything yet
                       , lang        :: [(String, String)]
                       , proj        :: [(String, String)]
                       , home        :: Maybe (IO String)  
                       } 

defaultOptions :: Options
defaultOptions = Options { 
                           interactive = False
                         , ext = extensionDict
                         , lang = []
                         , proj = []
                         , home = Just (getHomeDirectory)
                         , askOptions = False
                         , output = []}

options :: [ OptDescr (Options -> IO Options) ]
options = [
           Option "a" ["ask"]         (NoArg setAskOptions)       "Ask options interactively"
         , Option "h" ["help"]        (NoArg outputHelp)          "Output command info"
         , Option "H" ["home"]        (ReqArg setHome "HOME")     "Set HOME directory"
         , Option "i" ["interactive"] (NoArg setInteractive)      "Start interactive query session"
         , Option "l" ["language"]    (ReqArg setLanguage "PATH") "Set languages directory"
         , Option "o" ["output"]      (ReqArg setOutput "PATH")   "See file to output to"
         , Option "p" ["project"]     (ReqArg setProject "PATH")  "Set projects directory"
          ]


setAskOptions opt    = return opt { askOptions = True }
outputHelp _         = putStrLn (usageInfo usage options) >> exitWith ExitSuccess
setHome h opt        = return opt { home = Just (return h) }
setInteractive opt   = return opt { interactive = True, askOptions = True }
setLanguage path opt = return opt { lang = parseDescription path : (lang opt) } 
setProject  path opt = return opt { proj = parseDescription path : (proj opt) } 
setOutput path opt   = return opt { output = path : (output opt) } 


makeStatOptions :: Options -> IO StatOptions
makeStatOptions opts = do h <- fromMaybe (return "none") $ home opts
                          return SO { extensions = ext opts 
                                    , languages  = toMatches $ lang opts
                                    , projects   = toMatches $ proj opts
                                    , homePath   =  h }


main = do hSetBuffering stdout NoBuffering  -- remove LineBuffering from stdout
          args <- getArgs
          let (actions, nonOpts, msgs) = getOpt RequireOrder options args 
          when (msgs /= []) (error $ concat msgs)
          opts <- foldl (>>=) (return defaultOptions) actions
          so <- return $ makeStatOptions opts
          if null nonOpts
            then putStrLn usage
            else do so <- if askOptions opts then askStatOptions else so
                    s  <- statsFromFile (head nonOpts) so 
                    interactiveQueries s

usage = "Report generator\nCopyright 2009-2010, Bart Spaans\n\n  Usage: report [OPTIONS] LOG\n"
