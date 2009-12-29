module Stats ( EditStats(extInformation, language
                , project, fileName, editTime, edit) 
             , Stats, stats
             , both, startsWith
             , module Calendar
             , module StatOptions
             , CalendarS, CalendarSAlgebra, calendarS 
             , fromFile
             ) where 

import Edits
import StatOptions
import Maybe 
import Control.Applicative
import Data.List as L (groupBy, sort)
import qualified Data.Map as D hiding (map, filter, mapMaybe)
import System.FilePath
import Calendar



-- Stats — Data Structures
--
data EditStats  = FS { extInformation :: Maybe Description
                     , language       :: Maybe (Description, String)
                     , project        :: Maybe (Description, String)
                     , fileName       :: FilePath
                     , editTime       :: (Int, Int, Int)
                     , edit           :: Edit } deriving (Show, Eq)
type Stats      = [EditStats]



-- Stats — Build EditStats
--
stats      :: Edit -> Edit -> StatOptions -> EditStats
matchFile  :: Edit -> Matches -> Maybe (Description, Match)
diffEdit   :: Edit -> Edit -> (Int, Int, Int)
startsWith :: Eq a => [a] -> [a] -> Bool
both       :: (a -> b) -> a -> a -> (b, b)

slash :: String
slash = [pathSeparator]


stats e e2 (SO ext lang proj home) = FS ex l p n t e
  where ex               = D.lookup (takeExtension f) ext
        (l, p)           = both (fmap (\(a, (_, b)) -> (a, b))) la pr
        (la, pr)         = both (matchFile e) lang (maybe [] new la ++ proj)
        new (c, (fs, d)) = if d == slash then [] else [(fs ++ [d ++ slash] , d)]
        n                = if startsWith home f then replaceHome else f
        (t, f)           = (diffEdit e e2, file e)
        replaceHome      = '~' : drop (length home) f



matchFile e ps           = snd <$> (listToMaybe . reverse . sort $ matches)
  where matches          = mapMaybe (uncurry match) ps
        match fs c       = if check then j else Nothing
          where path     = if [last p] == slash then init p else slash
                check    = startsWith fs s && length s > length fs
                (s, p)   = (splitPath (file e), s !! length fs)
                j        = Just (length fs, (c, (fs, path))) 


-- Get the time difference between two Edits.
--
diffEdit (Edit _ _ _ h m s _ _ _) (Edit _ _ _ h' m' s' _ _ _) = res
  where res      = (div t ho, div (mod t ho) mi, mod (mod t ho) mi)
        t        = (h' - h) * ho + (m' - m) * mi + s' - s
        (mi, ho) = (60, mi * mi)



-- Helper functions
--
both f a b = (f a, f b)

startsWith _      []     = False
startsWith []     _      = True
startsWith (a:as) (b:bs) = a == b && startsWith as bs



-- Calendar of Stats 
--
type CalendarS  = Calendar Stats
type CalendarSAlgebra y m d r = CalendarAlgebra Stats y m d r

calendarEtoS   :: StatOptions -> CalendarE -> CalendarS
calendarS      :: StatOptions -> Edits -> CalendarS
fromFile       :: FilePath -> IO StatOptions -> IO CalendarS


calendarEtoS so = fmap stat 
     where stat = map (\e -> stats (head e) (last e) so) . fileGroup 

calendarS so    = calendarEtoS so . calendarE
fromFile f so   = calendarS <$> so <*> parseFile f


