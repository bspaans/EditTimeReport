module StatOptions ( StatOptions(..)                        -- StatOptions
                   , Description, Match, Matches            -- Type synonyms
                   , parseDescription, toMatches            -- Match parser for Main
                   ) where

import Control.Arrow
import Data.Char
import Data.List
import System.FilePath


-- StatOptions — Types
--
data StatOptions = SO { languages  :: Languages,   -- languages src locations 
                        projects   :: Projects,    -- projects src locations
                        homePath   :: String    }  -- HOME dir
type Description = String
type SplitPath   = [FilePath]
type Match       = (SplitPath, Description)
type Matches     = [Match]
type Languages   = Matches
type Projects    = Matches


-- Matches — Description Parsers
--
parseDescription :: String -> (String, String)
toMatches        :: [(String, String)] -> Matches


parseDescription [] = ("", "")
parseDescription s  = maybe noDesc desc getDesc 
  where reversed    = reverse s
        desc (x,y)  = (reverse x, reverse y)
        noDesc      = (s, dropTrailingPathSeparator . last . splitPath $ s)
        getDesc     = do c <- elemIndex ')' reversed
                         o <- elemIndex '(' reversed
                         if all isSpace (take c reversed) 
                           then return (drop (o + 1) reversed, 
                                        take (o + 1) (drop (c + 1) reversed))
                           else Nothing

toMatches = map (first $ splitPath . addTrailingPathSeparator)
