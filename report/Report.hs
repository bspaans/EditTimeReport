{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE UndecidableInstances #-}

module Report (
         Report, IndexedReport, SIndexedReport     -- Tasty Types
       , Indexed, IndexedY, IndexedM, IndexedD     -- Building indexed reports
       , SIndexed, SIndexedY, SIndexedM, SIndexedD -- Type abbr for Stats 
       , IndexedAlgebra, SIndexedAlgebra           -- Algebras
       , foldIR, report                            -- Fold and applied fold
       , indexAlgebra, indexFromFile               -- Indexed reports algebra
       , Table, TimeTable                          -- Tables
       , tableExtensions, tableLanguages           -- Tables
       , tableProjects, tableFilenames             -- Tables
       , tableYear, tableMonth                     -- Tables
       , tableDay, tableDayofWeek                  -- Tables
       , module Stats
       ) where


import Stats
import Text.PrettyPrint
import Control.Arrow
import Control.Applicative


-- Type Synonyms — Reports
--
type Report a = CalendarS -> a
type SReport  = Report SIndexedReport


-- Type Synonyms — IndexedReports
-- IndexedReports look like this:
--
--   [(a1, [(a2, [(a3, [b])])])]
--
-- So it can be used to store year, month
-- and day information in the parent node, 
-- while keeping the payload data in the leafs.
--
newtype IndexedReport a b = IR (Indexed a b) deriving (Eq, Show)
type    Indexed       a b = [IndexedY a b]
type    IndexedY      a b = (a, [IndexedM a b])
type    IndexedM      a b = (a, [IndexedD a b])
type    IndexedD      a b = (a, b)


-- Type Synonyms — SIndexedReports
-- Some abbreviations, because we work 
-- with Stats a lot
--
type SIndexedReport = IndexedReport Int Stats 
type SIndexed       = Indexed  Int Stats
type SIndexedY      = IndexedY Int Stats
type SIndexedM      = IndexedM Int Stats 
type SIndexedD      = IndexedD Int Stats 


-- Functors and Folds
-- fmap for `IndexedReports a b' applies a function 
-- to all b.
--
instance Functor (IndexedReport a) where 
  fmap f (IR ir) = IR (map (second (map (second (map (second f))))) ir)


type SIndexedAlgebra y m d r = IndexedAlgebra Int Stats y m d r
type IndexedAlgebra a b y m d r = 
                          ( [y] -> r       -- years
                          , (a, [m]) -> y  -- year
                          , (a, [d]) -> m  -- month
                          , (a, b) -> d)   -- day



-- Apply an IndexedAlgebra to an IndexedReport
--
foldIR :: IndexedAlgebra a b y m d r -> IndexedReport a b -> r
foldIR (ys, y, m, d) = years . fromIR 
         where years = ys . map (y . fmap (map (m . fmap (map d))))



-- Get something out of the IR context
--
fromIR :: IndexedReport a b -> Indexed a b
fromIR (IR ir) = ir



-- Algebra and Calendar fold applied — report
--
report        :: SReport
indexAlgebra  :: CalendarSAlgebra SIndexedY SIndexedM SIndexedD SIndexedReport
indexFromFile :: FilePath -> StatOptions -> IO SIndexedReport



report = foldCalendar indexAlgebra 

indexAlgebra   = (IR, y, mon, days)
  where y    m = first (year . edit . head . sh . sh) (m, m)
        mon  d = first (month . edit . head . sh) (d, d)
        days s = first (day . edit . head) (s, s)
        sh     = snd . head

indexFromFile f so  = report <$> fromFile f so 


-- Tables
-- Generates time tables from the groupings.
--
type Table a b = [(a, b)]
type TimeTable a = Table a Time

table           :: Ord a => (EditStats -> a) -> Report (TimeTable a)
tableExtensions :: Report (TimeTable (Maybe Description))
tableLanguages  :: Report (TimeTable (Maybe (Description, String)))
tableProjects   :: Report (TimeTable (Maybe (Description, String)))
tableFilenames  :: Report (TimeTable FilePath)
tableYear       :: Report (TimeTable Int)
tableMonth      :: Report (TimeTable Int)
tableDay        :: Report (TimeTable Int)
tableDayofWeek  :: Report (TimeTable Int)


table f         = map (f . head &&& sumTime) . grouping f 
tableExtensions = table extInformation
tableLanguages  = table language
tableProjects   = table project
tableFilenames  = table fileName
tableYear       = table (year . edit)
tableMonth      = table (month . edit)
tableDay        = table (day . edit)
tableDayofWeek  = table (dow . edit)


tableToTree :: TimeTable String -> StatsTree
tableToTree t = Root 2 nodes
  where nodes = map (\(s, time) -> Node 1 s [Leaf time]) t
