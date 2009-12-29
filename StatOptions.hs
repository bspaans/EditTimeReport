module StatOptions ( StatOptions(SO, extensions, languages -- StatOptions
                   , projects, homePath)                   -- StatOptions
				   , Description, Extension, Extensions    -- Type synonyms
                   , SplitPath, Match, Matches             -- Type synonyms
				   , Languages, Projects                   -- Type synonyms
				   , defaultSO, defaultIOSO                -- Debugging SO's
                   ) where

import qualified Data.Map as D hiding (map, filter, mapMaybe)
import System (getEnv)
import System.FilePath

type Description = String
type Extension = String
type Extensions = D.Map Extension Description


type SplitPath = [FilePath]
type Match = (SplitPath, Description)
type Matches = [Match]
type Languages = Matches
type Projects = Matches


data StatOptions = SO { extensions :: Extensions,
                        languages  :: Languages, 
                        projects   :: Projects,
                        homePath   :: String } 

extensionDict :: Extensions
extensionDict = D.fromList [(".txt", "Text File"), (".hs", "Haskell"), (".py", "Python"), 
                            (".tex", "LaTeX"), (".lua", "Lua"), (".sh", "Shell")]

defaultIOSO :: IO StatOptions
defaultIOSO = do home <- getEnv "HOME"
                 return $ SO extensionDict [] [] home

defaultSO :: StatOptions
defaultSO = SO extensionDict l p []


l = [(splitPath "/home/bspaans/coding/", "code")]
p = [(splitPath "/home/bspaans/docs/mydocs/", "docs")] 


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
