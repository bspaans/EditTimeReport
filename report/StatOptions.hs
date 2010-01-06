module StatOptions ( StatOptions(..)                       -- StatOptions
                   , Description, Extension, Extensions    -- Type synonyms
                   , SplitPath, Match, Matches             -- Type synonyms
                   , Languages, Projects                   -- Type synonyms
                   , parseDescription                      -- Match parser for Main
                   , askStatOptions                        -- Interactive session
                   , defaultSO, defaultIOSO                -- Debugging SO's
                   , extensionDict
                   ) where

import qualified Data.Map as D hiding (map, filter, mapMaybe)
import Data.Char
import Data.List
import Control.Monad
import Control.Arrow
import System
import System.FilePath
import System.Console.Editline.Readline
import Text.Printf



-- StatOptions — Data Type
--
data StatOptions = SO { extensions :: Extensions,  -- Extensions to match on
                        languages  :: Languages,   -- languages src locations 
                        projects   :: Projects,    -- projects src locations
                        homePath   :: String    }  -- HOME dir



-- StatOptions — Type synonyms
--
type Description = String
type Extension   = String
type Extensions  = D.Map Extension Description


type SplitPath   = [FilePath]
type Match       = (SplitPath, Description)
type Matches     = [Match]
type Languages   = Matches
type Projects    = Matches


-- Matches — Description Parsers
--
parseDescription :: String -> (String, String)
parseDescription [] = ("", "")
parseDescription s = case getDesc of 
                       Nothing -> (s, dropTrailingPathSeparator . last . splitPath $ s)
                       Just (x,y)  -> (reverse x, reverse y)
  where reversed = reverse s
        getDesc = do c <- elemIndex ')' reversed
                     o <- elemIndex '(' reversed
                     if all isSpace (take c reversed) 
                       then return (drop (o + 1) reversed, 
                            take (o + 1) (drop (c + 1) reversed))
                       else Nothing

-- Some defaults
--
extensionDict :: Extensions
defaultIOSO   :: IO StatOptions
defaultSO     :: StatOptions

extensionDict = D.fromList [(".txt", "Text File"), (".hs", "Haskell")
                          , (".py", "Python"), (".tex", "LaTeX")
                          , (".lua", "Lua"), (".sh", "Shell")]

defaultIOSO   = do home <- getEnv "HOME"
                   return $ SO extensionDict [] [] home

defaultSO     = SO extensionDict [] [] "none"




-- Interactive session 
-- Asking for StatOptions
--
askStatOptions    :: IO StatOptions


askStatOptions = do 
    home <- getEnv "HOME"
    putStrLn $ "  HOME set to " ++ home
    putStrLn "\n  ==================== Extensions ==================\n"
    putStrLn "  The following extensions are enabled by default:\n" 
    putStrLn $ printExtensions extensionDict
    exts <- askExtensions
    extDict <- return (D.union (D.fromList exts) extensionDict)
    when (exts /= []) (do putStrLn "  Using the following extensions" ; 
                          putStrLn $ printExtensions extDict)
    src  <- askSourceDir
    lang <- askLanguages
    putStrLn "\n"
    return (SO extDict lang src home)


-- Interactive — Ask helper functions
-- Ask question and check for valid answer
-- in assoc list.
--
ask               :: String -> [(String, a)] -> IO a
askNonEmpty       :: String -> IO String
askMore           :: IO Bool
askDesc           :: String -> String
askMultiple       :: IO t -> IO [t]


ask s d = do a <- readline s; 
             case a of 
                Nothing -> exitWith ExitSuccess
                Just x -> maybe n return $ lookup x d
  where n = putStrLn "\n  Not a valid option." >> ask s d


askNonEmpty s = do 
  a <- readline s
  case a of 
    Nothing -> exitWith ExitSuccess
    Just "" -> putStrLn "\n  Answer can't be empty" >> askNonEmpty s
    Just x  -> return x


askMore = do 
    ask "\n  Would you like to enter more (y/n) [N]? " a
    where a = [("y", True), ("n", False), ("", False)]


askDesc a       = "  Enter a description for `" ++ a ++ "': "

askMultiple = askOne []
  where askOne r a = do
           more <- askMore
           if more then do e <- a ; askOne (e:r) a
                   else return r


-- Printers
--
printExtensions   :: Extensions -> String
printMatches      :: Matches -> String

printExtensions = concatMap (uncurry $ printf "    %-7s %-14s\n") . D.assocs 
printMatches    = concatMap (uncurry (printf "    %-27s %-20s\n") . first joinPath)



-- Interactive — Ask for extensions
--
askExtensions     :: IO [(String, String)]
askExtension      :: IO (String, String)


askExtensions        = askMultiple askExtension
askExtension         = do e <- askE ; d <- askD e ; return (addD e, d)
  where askE         = askNonEmpty "  Enter a new extension (eg. txt): "
        askD         = askNonEmpty . askDesc 
        addD []      = []
        addD ('.':s) = '.':s
        addD xs      = '.':xs



-- Interactive — Ask matches
--
askMatch   :: IO Match
askMatches :: IO Matches

-- TODO: check for pathSeparator at end of path
--
askSourceDir = do putStrLn "\n\n  ================= Source Directories ============\n"
                  putStrLn "\n  If you have a directory containing different project directories, "
                  putStrLn "  you can let this program tag the edits in those directories with "
                  putStrLn "  their projects. For example: if you have a directory ~/src containing "
                  putStrLn "  directories ~/src/project1/ and ~/src/project2 you can enter ~/src."
                  putStrLn "  Currently there are no source directories specified."
                  m <- askMatches     
                  when (m /= []) (do putStrLn "\n\n  Using source directories:\n"; 
                                     putStrLn $ printMatches m)
                  return m


askLanguages = do putStrLn "\n\n  ==================== Languages ==================\n"
                  putStrLn "  If you have a directory ~/src containing directories "
                  putStrLn "  ~/src/python/ and ~/src/haskell/ you can enter ~/src/"
                  putStrLn "  and let the program figure out the language of each edit."
                  putStrLn "  Each language directory is also used as a source directory,"
                  putStrLn "  so ~/src/python/program/ automatically gets listed under "
                  putStrLn "  language `python' and project `program'."
                  putStrLn "  Currently there are no language directories specified."
                  m <- askMatches     
                  when (m /= []) (do putStrLn "\n\n  Using language directories:\n"; 
                                     putStrLn $ printMatches m)
                  return m



askMatches = askMultiple askMatch

askMatch = do p <- askP ; d <- askD p ; return (splitPath p, d)
  where askP = askNonEmpty "  Enter a new path: "
        askD = askNonEmpty . askDesc



{- StatOptions — Parser
 -
 -    INI        -> ε | Sections
 -    Sections   -> Section | Sections Section
 -    Section    -> Header Parameters
 -    Header     -> [ S ]
 -    Parameters -> Parameter | Parameters Parameter
 -    Parameter  -> ε | Token = Value
 -    S          -> "Extensions" | "Languages" | "Projects"
 -    Token      -> [a-zA-Z0-9]*
 -    Value      -> ^\n*
 -    -}
