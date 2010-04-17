-- | Provides a low level view of the log file.

module Edits ( -- * AST
               Edits, Edit(..),
               -- ** Parsing
               editsFromFile,        
               -- * Grouping
               groupWith , fileGroup,
               -- ** Predicates
               Pred,
               -- * Calendars
               Calendar(..), flatten,              
               -- ** Calendar of Edits
               calendarE, CalendarE  
             ) where


import Char
import Maybe (mapMaybe)
import Control.Applicative
import Data.List as L (groupBy)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import qualified Data.Map as D hiding (map, filter, mapMaybe)
import System.FilePath



-- | Abstract syntax for an editor log entry.
--
data Edit  = Edit {   year    :: Int
                    , month   :: Int
                    , week    :: Int
                    , day     :: Int
                    , hour    :: Int
                    , minute  :: Int
                    , seconds :: Int
                    , dow     :: Int     -- ^ Day of the week
                    , doy     :: Int     -- ^ Day of the year
                    , file    :: FilePath } deriving (Show, Eq)
type Edits = [Edit]




-- | Parses log files generated by editlogd
-- daemon or editor.
--
editsFromFile :: FilePath -> IO Edits
editsFromFile f = parseEdits <$> readFile f


parseEdits :: String   -> Edits
parseEdit  :: String   -> Maybe Edit

parseEdits  = mapMaybe parseEdit . lines
parseEdit s = if a then Just $ Edit y m we d h mi se dw dy (unwords (drop 9 w))
                   else Nothing
                   where w = words s
                         (_, we, _) = toWeekDate (fromGregorian (toInteger y) m d)
                         i = take 8 w
                         a = length w >= 9 && all (all isDigit) i && w !! 8 == "EDIT"
                         (y:m:d:h:mi:se:dw:dy:[]) = map read i


-- | A simple abstraction for predicates 
--
type Pred a = a -> Bool




-- | Use a function's result to group by.
--
groupWith  :: Eq a => (b -> a) -> [b] -> [[b]]
groupWith y = groupBy (\f g -> y f == y g)

-- | Group Edits on year.
--
yearGroup  :: Edits -> [Edits]
yearGroup   = groupWith year

-- | Group Edits on month.
--
monthGroup :: Edits -> [Edits]
monthGroup  = groupWith month

-- | Group Edits on day of the month.
--
dayGroup   :: Edits -> [Edits]
dayGroup    = groupWith day

-- | Group Edits on file name.
--
fileGroup  :: Edits -> [Edits]
fileGroup   = groupWith file


-- | The Calendar type is an abstraction over lists of days in 
-- lists of months in lists of years:
--
-- >      [ [ [ a ] ] ]
-- >              | | + years
-- >              | + months
-- >              + days  
--
newtype Calendar a = Calendar [[[a]]] deriving (Eq, Show)

instance Functor Calendar where
  fmap f (Calendar as) = Calendar (map (map (map f)) as)


-- Getting items out of the Calendar container
--
fromCalendar :: Calendar a -> [[[a]]]
flatten      :: Calendar a -> [a]

fromCalendar (Calendar as) = as
flatten = concat . concat . fromCalendar


-- Calendar of Edits
-- Using the grouping functions to build it.
-- Expects a chronological list of edits.
--
type CalendarE = Calendar Edits

calendarE :: Edits -> CalendarE
calendarE = Calendar . map (map dayGroup . monthGroup) . yearGroup 
