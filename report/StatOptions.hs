module StatOptions ( StatOptions(SO, extensions, languages -- StatOptions
                   , projects, homePath)                   -- StatOptions
                   , Description, Extension, Extensions    -- Type synonyms
                   , SplitPath, Match, Matches             -- Type synonyms
                   , Languages, Projects                   -- Type synonyms
                   , defaultSO, defaultIOSO                -- Debugging SO's
                   ) where

import qualified Data.Map as D hiding (map, filter, mapMaybe)
import Text.Printf
import System (getEnv)
import System.FilePath
import Control.Monad



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
ask               :: String -> [(String, a)] -> IO a
askNonEmpty       :: String -> IO String


askStatOptions = do 
    home <- getEnv "HOME"
    putStrLn $ "  HOME set to " ++ home
    putStrLn "  The following extensions are enabled by default:\n" 
    putStrLn $ printExtensions extensionDict
    exts <- askExtensions
    extDict <- return (D.union (D.fromList exts) extensionDict)
    when (exts /= []) (do putStrLn "  Using the following extensions" ; 
                          putStrLn $ printExtensions extDict)
    return (SO extDict [] [] home)


-- Ask question and check for valid answer
-- in assoc list.
--
ask s d = do putStrLn $ s ++ "\n"; getAnswer
   where getAnswer = do a <- getLine ; maybe n return $ lookup a d
         n         = putStrLn "  Not a valid option.\n" >> getAnswer


askNonEmpty s = do 
  putStrLn s
  a <- getLine
  if a == "" 
    then putStrLn "  Answer can't be empty\n" >> askNonEmpty s
    else return a



-- Interactive — Ask for extensions
--
askMoreExtensions :: IO Bool
askExtensions     :: IO [(String, String)]
askExtension      :: IO (String, String)
printExtensions   :: Extensions -> String


askMoreExtensions = do 
    ask "\n  Would you like to enter more (y/n) [N]?" a
    where a = [("y", True), ("n", False), ("", False)]


askExtensions = askExts []
  where askExts r = do more <- askMoreExtensions
                       if more then do e <- askExtension ; askExts (e:r)
                               else return r


askExtension            = do ext <- askExt ; desc <- askDesc ext ; return (addDot ext, desc)
  where askExt          = askNonEmpty "\n  Enter a new extension (eg. txt)\n"
        askDesc a       = askNonEmpty $ "\n  Enter a description for extension `" ++ a ++ "'\n"
        addDot []       = []
        addDot ('.':xs) = '.':xs
        addDot xs       = '.':xs


printExtensions = concatMap (uncurry $ printf "    %-7s %-14s\n") . D.assocs 




-- Specify language and project options like this:
--
--l = [(splitPath "/home/bspaans/coding/", "code")]
--p = [(splitPath "/home/bspaans/docs/mydocs/", "docs")] 


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
