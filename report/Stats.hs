module Stats ( EditStats(extInformation, language        -- EditStats
                , project, fileName, editTime, edit)     -- EditStats
             , Stats, stats                              -- Stats
             , both, startsWith                          -- Handy functions
             , Time, Seconds, fromSeconds, toSeconds     -- Time
             , sumTime, diffEdit                         -- Time
             , CalendarS, CalendarSAlgebra, calendarS    -- CalendarS + algebra
             , fromFile                                  -- Stats from file
             , module Calendar , module StatOptions
             ) where 

import Edits
import StatOptions
import Maybe 
import Control.Applicative
import Control.Arrow
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
startsWith :: Eq a => [a] -> [a] -> Bool
both       :: (a -> b) -> a -> a -> (b, b)

slash :: String
slash = [pathSeparator]


stats e e2 (SO ext lang proj home) = FS ex l p n t e
  where ex               = D.lookup (takeExtension f) ext
        (l, p)           = both (fmap (second snd)) la pr
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



-- Times
--
diffEdit    :: Edit -> Edit -> (Int, Int, Int)
toSeconds   :: Time -> Seconds
fromSeconds :: Seconds -> Time
sumTime     :: Stats -> (Int, Int, Int)


type Seconds        = Int
type Time           = (Int, Int, Int)


diffEdit (Edit _ _ _ h m s _ _ _) (Edit _ _ _ h' m' s' _ _ _) = fromSeconds t
     where t        = toSeconds (h', m', s') - toSeconds (h, m , s)


toSeconds (h, m, s) = h * 3600 + m * 60 + s
fromSeconds t       = (div t 3600, div (mod t 3600) 60, mod (mod t 3600) 60)
sumTime             = fromSeconds . sum . map (toSeconds . editTime)



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


