module Main where


import Query
import Stats

import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Maybe
import System 
import System.Console.GetOpt
import System.FilePath
import System.Directory


data Options = Options {
                         interactive :: Bool
                       , askOptions  :: Bool
                       , ext         :: Extensions          
                       , lang        :: [(String, String)]
                       , proj        :: [(String, String)]
                       , home        :: Maybe (IO String)  
                       , format      :: [String]
                       , commands    :: [String]
                       } 

defaultOptions :: Options
defaultOptions = Options { interactive = False
                         , ext = extensionDict
                         , lang = []
                         , proj = []
                         , home = Just (getHomeDirectory)
                         , askOptions = False
                         , format = []
                         , commands = []
                         }

options :: [ OptDescr (Options -> IO Options) ]
options = [
           Option "a" ["ask"]         (NoArg setAskOptions)       "Ask options interactively"
         , Option "c" ["command"]     (ReqArg addCommand "CMD")   "Evaluate command."
         , Option "h" ["help"]        (NoArg outputHelp)          "Output command info"
         , Option "f" ["format"]      (ReqArg setFormat "FORMAT") "Set default format (csv, html, text, xhtml)"
         , Option "H" ["home"]        (ReqArg setHome "HOME")     "Set HOME directory"
         , Option "i" ["interactive"] (NoArg setInteractive)      "Start interactive query session"
         , Option "l" ["language"]    (ReqArg setLanguage "PATH") "Set languages directory"
         , Option "p" ["project"]     (ReqArg setProject "PATH")  "Set projects directory"
          ]


setAskOptions opt    = return opt { askOptions = True }
addCommand c opt     = return opt { commands = c : commands opt }
outputHelp _         = putStrLn (usageInfo usage options) >> exitWith ExitSuccess
setFormat f opt      = return opt { format = f : format opt } 
setHome h opt        = return opt { home = Just (return h) }
setInteractive opt   = return opt { interactive = True }
setLanguage path opt = return opt { lang = parseDescription path : (lang opt) } 
setProject  path opt = return opt { proj = parseDescription path : (proj opt) } 


makeStatOptions :: Options -> IO StatOptions
makeStatOptions opts = do h <- fromMaybe (return "none") $ home opts
                          return SO { extensions = ext opts 
                                    , languages  = toMatches $ lang opts
                                    , projects   = toMatches $ proj opts
                                    , homePath   =  h }

makePrintOptions :: Options -> E PrintOptions
makePrintOptions opts = setPrinters (format opts) 


main = do args <- getArgs
          let (actions, nonOpts, msgs) = getOpt RequireOrder options args 
          when (msgs /= []) (error $ concat msgs)
          opts <- foldl (>>=) (return defaultOptions) actions
          so <- return $ makeStatOptions opts
          po <- case makePrintOptions opts of
                  Failed s -> error s
                  Ok a -> return a
          if null nonOpts
            then putStrLn usage
            else do so <- if askOptions opts then askStatOptions else so
                    stats <- statsFromFile (head nonOpts) so
                    when (not . null $ commands opts) (putStr $ execute' emptyEnv po (commands opts) stats)
                    when (length nonOpts > 1) (do co <- commandsFromFiles emptyEnv (tail nonOpts)
                                                  putStr $ execute emptyEnv po co stats)
                    when (interactive opts) (interactiveQueries po stats)

usage = "Report generator\nCopyright 2009-2010, Bart Spaans\n\n  Usage: report [OPTIONS] LOGFILE [QUERY [QUERY ..]]\n"
