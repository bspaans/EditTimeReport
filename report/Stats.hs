module Stats ( EditStats(..), Stats, Header     -- Types
             , StatOptions(..)                  -- StatOptions
             , both, startsWith                 -- Handy functions
             , Time, sumTime, sumTime'          -- Time
             , StatsTree(..)                    -- Tree 
             , StatsTreeAlgebra, foldTree       -- Tree
             , statsFromFile                    -- Stats from file
             , parseDescription, toMatches      -- Match parser for Main
             , module Edits
             ) where 

import Edits
import Control.Applicative
import Control.Arrow
import Data.Char
import Data.List
import Data.Maybe 
import qualified Data.Map as D hiding (map, filter, mapMaybe)
import System.FilePath


-- Stats — Data Structures
--
data EditStats  = ES { language       :: Maybe (Description, String)
                     , project        :: Maybe (Description, String)
                     , fileName       :: FilePath
                     , editTime       :: (Int, Int, Int)
                     , edit           :: Edit } deriving (Show, Eq)
type Stats      = [EditStats]


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


-- Stats — Build EditStats
--
stats      :: Edit -> Edit -> StatOptions -> EditStats
matchFile  :: Edit -> Matches -> Maybe (Description, Match)
startsWith :: Eq a => [a] -> [a] -> Bool
both       :: (a -> b) -> a -> a -> (b, b)


-- Create EditStats (ie match languages, projects and 
-- home directory, count edit time) from two Edits
--
stats e e2 (SO lang proj home) = ES l p n t e
  where (l, p)           = both (fmap (second snd)) la pr
        (la, pr)         = both (matchFile e) lang (maybe [] new la ++ proj)
        new (c, (fs, d)) = if d == slash then [] else [(fs ++ [d ++ slash] , d)]
        n                = if startsWith home f then replaceHome else f
        (t, f)           = (diffEdit e e2, file e)
        replaceHome      = '~' : drop (length home) f


-- Try to match a File with a Project or Language
--
matchFile edit matches'  = snd <$> (listToMaybe . reverse . sort $ matches)
  where matches          = mapMaybe (uncurry match) matches'
        match fs c       = if check then j else Nothing
          where path     = if [last p] == slash then init p else slash
                check    = startsWith fs s && length s > length fs
                (s, p)   = (splitPath (file edit), s !! length fs)
                j        = Just (length fs, (c, (fs, path))) 


slash :: String
slash = [pathSeparator]


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


-- Times
--
diffEdit    :: Edit -> Edit -> Time
toSeconds   :: Time -> Seconds
fromSeconds :: Seconds -> Time
sumTime     :: Stats -> Time
sumTime'    :: [Time] -> Time


type Seconds        = Int
type Time           = (Int, Int, Int)


diffEdit (Edit _ _ _ h m s _ _ _) (Edit _ _ _ h' m' s' _ _ _) = fromSeconds t
     where t        = toSeconds (h', m', s') - toSeconds (h, m , s)


toSeconds (h, m, s) = h * 3600 + m * 60 + s
fromSeconds t       = (div t 3600, div (mod t 3600) 60, mod (mod t 3600) 60)
sumTime             = fromSeconds . sum . map (toSeconds . editTime)
sumTime'            = fromSeconds . sum . map toSeconds 


-- Helper functions
--
both f a b = (f a, f b)

startsWith _      []     = False
startsWith []     _      = True
startsWith (a:as) (b:bs) = a == b && startsWith as bs



-- Calendar of Stats 
--
type CalendarS  = Calendar Stats

calendarEtoS   :: StatOptions -> CalendarE -> CalendarS
calendarS      :: StatOptions -> Edits -> CalendarS
statsFromFile  :: FilePath -> StatOptions -> IO Stats

calendarEtoS so = fmap stat 
     where stat = map (\e -> stats (head e) (last e) so) . fileGroup 

calendarS so       = calendarEtoS so . calendarE
statsFromFile f so = concat . flatten . calendarS so <$> editsFromFile f



-- Stats — Trees
--
type Children  = Int -- Total number of children in tree
type Header    = String
type Title     = String
data StatsTree = Root [StatsTree] [Header] String Time
               | Node Children Time String [StatsTree] 
               | Leaf Time
                    deriving (Eq, Show)

type StatsTreeAlgebra r n = ([n] -> [Header] -> String -> Time -> r 
                          , Int -> Time -> String -> [n] -> n
                          , Time -> n)

foldTree :: StatsTreeAlgebra r n -> StatsTree -> r
foldTree (root, node, leaf) (Root tr h s t) = root (map f tr) h s t
  where f (Node i t s tr) = node i t s (map f tr)
        f (Leaf e)      = leaf e

