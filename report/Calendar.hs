module Calendar ( Calendar(Calendar)    -- Data structure
                , flatten, fromCalendar -- Calendars
                , CalendarAlgebra       -- Algebra
                , foldCalendar          -- foldCalendar
                , calendarE, CalendarE  -- Calendar of Edits
                , CalendarEAlgebra      -- Algebra
                , module Edits
                ) where 

import Edits

import Maybe 
import Control.Applicative
import Data.List as L (groupBy, sort)
import qualified Data.Map as D hiding (map, filter, mapMaybe)
import System.FilePath



-- The Calendar type
--
-- The Calendar type is an abstraction over lists of days in 
-- lists of months in lists of years:
--
--      [ [ [ a ] ] ]
--              | | + years
--              | + months
--              + days  
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


-- Calendar — Algebra and Fold
--
type CalendarAlgebra a y m d r = 
                           ( [y] -> r  -- years
                           , [m] -> y  -- year
                           , [d] -> m  -- month
                           , a -> d)   -- day

foldCalendar :: CalendarAlgebra a y m d r -> Calendar a -> r
foldCalendar (ys, y, m, d) = ys . map (y . map (m . map d)) . fromCalendar


-- Calendar of Edits
-- Using the grouping functions to build it.
-- Expects a chronological list of edits.
--
type CalendarE = Calendar Edits
type CalendarEAlgebra y m d r = CalendarAlgebra Edits y m d r

calendarE :: Edits -> CalendarE
calendarE = Calendar . map (map dayGroup .monthGroup) . yearGroup 
