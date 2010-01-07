module Edits ( Edit(..), Edits                 -- AST
             , editsFromFile                   -- parsing
             , Pred, PredE                     -- predicates
             , selEq, selCombine, selCombine'  -- predicates
             , comb, comb'                     -- combinators
             , onYear, onMonth, onDay          -- filtering
             , groupWith, yearGroup            -- grouping
             , monthGroup, dayGroup            -- grouping
             , fileGroup                       -- grouping
             ) where


import Char
import Maybe (mapMaybe)
import Control.Applicative
import Data.List as L (groupBy)
import qualified Data.Map as D hiding (map, filter, mapMaybe)
import System.FilePath



-- Edits — Data Structures
-- Abstract syntax for an editor log file.
--
data Edit  = Edit {   year    :: Int
                    , month   :: Int
                    , day     :: Int
                    , hour    :: Int
                    , minute  :: Int
                    , seconds :: Int
                    , dow     :: Int     -- Day of the week
                    , doy     :: Int     -- Day of the year
                    , file    :: FilePath } deriving (Show, Eq)
type Edits = [Edit]



-- Edits — Parsers
-- Parses log files generated by timechartd.py
--
parseEdit     :: String   -> Maybe Edit
parseEdits    :: String   -> Edits
editsFromFile :: FilePath -> IO Edits


parseEdit s = if a then Just $ Edit y m d h mi se dw dy (unwords (drop 9 w))
                   else Nothing
                   where w = words s
                         i = take 8 w
                         a = length w >= 9 && all (all isDigit) i && w !! 8 == "EDIT"
                         (y:m:d:h:mi:se:dw:dy:[]) = map read i

parseEdits  = mapMaybe parseEdit . lines
editsFromFile f = parseEdits <$> readFile f



-- Predicates 
--
type Pred a = a -> Bool
type PredE  = Pred Edit

selEq        :: Eq b => (a -> b) -> b -> Pred a
selCombine   :: Eq a => Pred a-> Pred a -> Pred a
selCombine'  :: Eq a => Pred a -> a -> Pred a
comb         :: (a -> b -> c) -> (i -> a) -> (i -> b) -> i -> c
comb'        :: (a -> a -> c) -> (i -> a) -> i -> i -> c


selEq f a      = (==a) . f
selCombine     = comb (==)
selCombine'    = comb' (==) 
comb  op f g a = f a `op` g a
comb' op f a b = f a `op` f b


-- Edits — Filtering 
-- Selecting edits on years, months and/or days.
--
yearEq  :: Int -> PredE
monthEq :: Int -> Int -> PredE
dayEq   :: Int -> Int -> Int -> PredE

onYear  :: Int -> Edits -> Edits
onMonth :: Int -> Int -> Edits -> Edits
onDay   :: Int -> Int -> Int -> Edits -> Edits


yearEq    = selEq year
monthEq y = selCombine (yearEq y) . selEq month
dayEq y m = selCombine (monthEq y m) . selEq day 

onYear    = filter . yearEq
onMonth y = filter . monthEq y 
onDay y m = filter . dayEq y m 



-- Edits — Grouping
-- Group edits on eg. year, month, day, file
-- 

groupWith  :: Eq a => (b -> a) -> [b] -> [[b]]
yearGroup  :: Edits -> [Edits]
monthGroup :: Edits -> [Edits]
dayGroup   :: Edits -> [Edits]
fileGroup  :: Edits -> [Edits]

groupWith y = groupBy (\f g -> y f == y g)
yearGroup   = groupWith year
monthGroup  = groupWith month
dayGroup    = groupWith day
fileGroup   = groupWith file

