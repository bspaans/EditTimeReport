module Main where


-- Currently only implemented HTML reports
--
import Printers
import HtmlPrinter
import Query
import Char
import List
import System 
import System.Console.GetOpt
import System.IO
import Control.Monad


data Options = Options {
                         interactive :: Bool
                       , askOptions  :: Bool
                       , ext         :: Extensions          -- <not doing anything yet>
                       , output      :: [String]
                       , lang        :: [(String, String)]
                       , proj        :: [(String, String)]
                       , home        :: Maybe String        -- </not doing anything yet>
                       } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options { 
                           interactive = False
                         , ext = extensionDict
                         , lang = []
                         , proj = []
                         , home = Nothing 
                         , askOptions = False
                         , output = []}

options :: [ OptDescr (Options -> IO Options) ]
options = [
           Option "a" ["ask"]         (NoArg setAskOptions)       "Ask options interactively"
         , Option "h" ["help"]        (NoArg outputHelp)          "Output command info"
         , Option "H" ["home"]        (ReqArg setHome "HOME")     "Set HOME directory"
         , Option "i" ["interactive"] (NoArg setInteractive)      "Start interactive query session"
         , Option "l" ["language"]    (ReqArg setLanguage "PATH") "Set language directory"
         , Option "o" ["output"]      (ReqArg setOutput "PATH")   "See file to output to"
          ]


setAskOptions opt    = return opt { askOptions = True }
outputHelp _         = putStrLn (usageInfo usage options) >> exitWith ExitSuccess
setHome h opt        = return opt { home = Just h }
setInteractive opt   = return opt { interactive = True, askOptions = True }
setLanguage path opt = return opt { lang = parseDescription path : (lang opt) } 
setOutput path opt   = return opt { output = path : (output opt) } 

parseDescription :: String -> (String, String)
parseDescription s = case getDesc of 
                       Nothing -> (s, "")
                       Just x  -> x
  where reversed = reverse s
        getDesc = do c <- elemIndex ')' reversed
                     o <- elemIndex '(' reversed
                     if all isSpace (take c reversed) 
                       then return (drop (o + 1) reversed, 
                            take (o - c + 1) (drop (c + 1) reversed))
                       else Nothing

main = do hSetBuffering stdout NoBuffering  -- remove LineBuffering from stdout
          args <- getArgs
          let (actions, nonOpts, msgs) = getOpt RequireOrder options args 
          when (msgs /= []) (error $ concat msgs)
          opts <- foldl (>>=) (return defaultOptions) actions
          po <- return defaultPO
          if null nonOpts
            then putStrLn usage
            else do so <- if askOptions opts then askStatOptions else defaultIOSO
                    s  <- statsFromFile (head nonOpts) so 
                    interactiveQueries s

usage = "Report generator\nCopyright 2009-2010, Bart Spaans\n\n  Usage: report [OPTIONS] LOG\n"
