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


data Options = Options { statO       :: IO StatOptions
                       , printO      :: PrintOptions
                       , interactive :: Bool
                       , ext         :: Extensions
                       , lang        :: [(String, String)]
                       , proj        :: [(String, String)]
                       , home        :: Maybe String
                       }

defaultOptions :: Options
defaultOptions = Options { statO  = defaultIOSO
                         , printO = defaultPO 
                         , interactive = False
                         , ext = extensionDict
                         , lang = []
                         , proj = []
                         , home = Nothing }

options :: [ OptDescr (Options -> IO Options) ]
options = [
           Option "h" ["help"]        (NoArg outputHelp)          "Output command info"
         , Option ""  ["home"]        (ReqArg setHome "HOME")     "Set HOME directory"
         , Option "i" ["interactive"] (NoArg setInteractive)      "Start interactive query session"
         , Option "l" ["language"]    (ReqArg setLanguage "PATH") "Set language directory"
          ]


outputHelp _       = do putStrLn $ usageInfo usage options 
                        exitWith ExitSuccess
setHome h opt      = return opt { home = Just h }
setInteractive opt = do putStrLn "Enabling interactive session" 
                        return opt { interactive = True }

setLanguage path opt = return opt { lang = parseDescription path : (lang opt) } 

parseDescription :: String -> (String, String)
parseDescription s = case getIndices of 
                       Nothing -> (s, "")
                       Just x  -> x
  where reversed = reverse s
        getIndices = do c <- elemIndex ')' reversed
                        o <- elemIndex '(' reversed
                        if all isSpace (take c reversed) 
                          then return (drop (o + 1) reversed, 
                               take (o - c + 1) (drop (c + 1) reversed))
                          else Nothing

-- TODO getOpt here
--
main = do hSetBuffering stdout NoBuffering  -- remove LineBuffering from stdout
          args <- getArgs
          let (actions, nonOpts, msgs) = getOpt RequireOrder options args 
          opts <- foldl (>>=) (return defaultOptions) actions
          po <- return defaultPO
          if length args < 1 
            then putStrLn usage
            else do so <- askStatOptions
                    let iqueries = do s <- statsFromFile (head args) so ; interactiveQueries s
                    if length args == 1 then iqueries
                                        else if length args == 2 && args !! 1 /= "-i" 
                                               then htmlFromFile (head args) po so >>= writeFile (args !! 1)
                                               else iqueries

usage = "Report generator\nCopyright 2009-2010, Bart Spaans\n\n  Usage: report LOG [OUTPUT]\n"
