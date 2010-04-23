module Main where


import Query
import Stats

import QueryAST
import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Maybe
import System 
import System.Console.GetOpt
import System.FilePath
import System.Directory


usage = unlines ["Report generator\nCopyright 2009-2010, Bart Spaans\n",
        "Usage: report [OPTIONS] LOGFILE [QUERYFILE..]]\n"]

data Options = Options {
                         interactive :: Bool
                       , askOptions  :: Bool
                       , lang        :: [(String, String)]
                       , proj        :: [(String, String)]
                       , home        :: Maybe (IO String)  
                       , format      :: [String]
                       , commands    :: [String]
                       } 

defaultOptions :: Options
defaultOptions = Options { interactive = False
                         , lang = []
                         , proj = []
                         , home = Just (getHomeDirectory)
                         , askOptions = False
                         , format = []
                         , commands = []
                         }

options :: [ OptDescr (Options -> IO Options) ]
options = [Option "h" ["help"]        (NoArg outputHelp)          "Output command info"
         , Option "f" ["format"]      (ReqArg setFormat "FORMAT") "Set default format (csv, html, text, xhtml)"
         , Option "H" ["home"]        (ReqArg setHome "HOME")     "Set HOME directory"
         , Option "i" ["interactive"] (NoArg setInteractive)      "Start interactive query session"
         , Option "l" ["language"]    (ReqArg setLanguage "PATH") "Set languages directory"
         , Option "q" ["query"]     (ReqArg addCommand "QUERY")   "Evaluate query."
         , Option "p" ["project"]     (ReqArg setProject "PATH")  "Set projects directory"
          ]


addCommand c opt     = return opt { commands = c : commands opt }
outputHelp _         = putStrLn (usageInfo usage options) >> exitWith ExitSuccess
setFormat f opt      = return opt { format = f : format opt } 
setHome h opt        = return opt { home = Just (return h) }
setInteractive opt   = return opt { interactive = True }
setLanguage path opt = return opt { lang = parseDescription path : (lang opt) } 
setProject  path opt = return opt { proj = parseDescription path : (proj opt) } 


makeStatOptions :: Options -> IO StatOptions
makeStatOptions opts = do h <- fromMaybe (return "none") $ home opts
                          return SO { languages  = toMatches $ lang opts
                                    , projects   = toMatches $ proj opts
                                    , homePath   =  h }

makePO :: Options -> E PrintOptions
makePO = setPrinters . format 


main = do 
    args <- getArgs
    let (actions, nonOpts, msgs) = getOpt RequireOrder options args 
    when (msgs /= []) (error $ concat msgs)
    opts <- foldl (>>=) (return defaultOptions) actions
    so <- makeStatOptions opts
    po <- case makePO opts of { Failed s -> error s ; Ok a -> return a }
    if null nonOpts then putStrLn usage else exec' so po opts nonOpts


exec' so po opts nonOpts= 
  do stats <- statsFromFile (head nonOpts) so
     let i = interactiveQueries po stats
     if not . null $ commands opts
       then putStr $ execute' emptyEnv po (commands opts) stats 
       else when (length nonOpts <= 1 && not (interactive opts)) (i emptyEnv)
     if length nonOpts > 1 
       then do co <- runET $ commandsFromFiles emptyEnv (tail nonOpts)
               case co of 
                 Failed s   -> error s
                 Ok (st, e) -> do putStr $ execute emptyEnv po co stats 
                                  when (interactive opts) (i e)
       else when (interactive opts) (i emptyEnv)

