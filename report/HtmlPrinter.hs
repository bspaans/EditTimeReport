module HtmlPrinter ( html          -- Convert SIndexedReports to Html table
                   , printReport   -- Making an instance of Printer
                   , htmlFromFile  -- Pretty printed Html
                   ) where

import Printers
import Text.XHtml hiding (header)
import Control.Applicative
import Data.List
import Data.Function



-- Converting SIndexedReports to tables using its fold
--
html             :: PrintOptions -> SIndexedReport -> Html
htmlAlgebra      :: SIndexedAlgebra Html Html Html Html
htmlFromFile     :: FilePath -> PrintOptions -> StatOptions -> IO String


html p            = foldIR htmlAlgebra
htmlAlgebra       = (concatHtml, showYear, showMonth, showDay)
htmlFromFile f p s= prettyHtml <$> (printReport p <$> fromFile f s :: IO Html)


instance Printer Html where
  printReport opts cs = reportStyle +++ year +++ mont +++ day +++ dow
          +++ exts +++ lang +++ proj +++ name +++ ind
    where exts = test PrintExtensionTable htmlExtensionTable
          lang = test PrintLanguageTable htmlLanguageTable
          proj = test PrintProjectTable htmlProjectTable
          name = test PrintFilenameTable htmlFilenameTable 
          ind  = test PrintSIndexed  (\o f -> html o . report $ f)
          year = test PrintYearTable htmlYearTable
          mont = test PrintMonthTable htmlMonthTable
          day  = test PrintDayTable htmlDayTable
          dow  = test PrintDayofWeekTable htmlDayofWeekTable
          test p f = if p `isSet` opts then f opts cs else noHtml


--Convert years, months and days to Html
--
showYear  :: (Int, [Html]) -> Html
showMonth :: (Int, [Html]) -> Html
showDay   :: (Int, Stats ) -> Html


showYear (year, months) = h3 (str year) +++ concatHtml months
showMonth (month, days) = h4 (toHtml $ getMonth month) +++ table (concatHtml days)
showDay (day, stats) = tr (td (str day) +++ td (table $ header +++ map showEntry stats))



-- Converting EditStats to Html
--
showEntry     :: EditStats -> Html
showExt       :: EditStats -> Html
showLanguage' :: EditStats -> Html
showProject'  :: EditStats -> Html
showTime'     :: EditStats -> Html


showEntry s      = tr (concatHtml $ map a funcs)
  where a (f, c) = td (f s) ! [theclass c]
        funcs    = [(showTime', "time")
                  , (toHtml . fileName, "filename")
                  , (showExt, "extension")
                  , (showLanguage' , "language" )
                  , (showProject' , "project") ]


showTime' s      = if editTime s == (0, 0, 0) then noHtml else sp 
   where sp      = thespan (toHtml (showTime $ editTime s)) 


showExt           = maybe noHtml (toSpan . brackets) . extInformation
showLanguage'     = maybe noHtml (showSub' "matchlang") . language 
showProject'      = maybe noHtml (showSub' "matchproj") . project 
showSub'    c s   = toSpan (showSub s) ! [theclass c]



-- Tables
--
type HReport = PrintOptions -> Report Html
htmlTable          :: String -> Report (TimeTable a) -> (a -> Html) -> HReport
htmlExtensionTable :: HReport
htmlLanguageTable  :: HReport
htmlProjectTable   :: HReport
htmlFilenameTable  :: HReport
htmlYearTable      :: HReport
htmlMonthTable     :: HReport
htmlDayTable       :: HReport
htmlDayofWeekTable :: HReport


htmlTable s r c op u = hr +++ p (table (h +++ (concatHtml . map row $ process u)))
  where row (d, t)   = tr (td (c d) +++ td (toHtml . showTime $ t))
        h            = tr (th  (toHtml s) +++ th (toHtml "Time"))
        process      = if SortedAsc `isSet` op then sortBy (compare `on` snd) . r
                         else if SortedDesc `isSet` op then reverse . sortBy (compare `on` snd) . r
                                else r


htmlExtensionTable   = htmlTable "Extension" tableExtensions ext 
  where ext          = toSpan . showExtension "NONE" 

htmlLanguageTable    = htmlTable "Language" tableLanguages lang 
  where lang         = toHtml . showLanguage "NONE"

htmlProjectTable     = htmlTable "Project" tableProjects proj
  where proj         = toHtml . showProject "NONE"

htmlFilenameTable    = htmlTable "Filename" tableFilenames toHtml
htmlYearTable        = htmlTable "Year" tableYear str 
htmlMonthTable       = htmlTable "Month" tableMonth (toHtml . getMonth) 
htmlDayTable         = htmlTable "Day" tableDay str 
htmlDayofWeekTable   = htmlTable "Day of the Week" tableDayofWeek (toHtml . getDow) 


-- StatsTree to Html table
--
treeToHtml :: StatsTree -> Html
treeToHtml = foldTree (root, node, leaf)
  where root ns = table (concatHtml . map tr $ ns)
        node cspan s ns = (td (toHtml s) ! [colspan cspan]) +++ table (concatHtml (map tr ns))
        leaf = td . toHtml . showTime


-- Helper functions
--
header      :: Html
toSpan      :: String -> Html
reportStyle :: Html

header      = tr (concatHtml $ map (th . toHtml) headers)
str         = toHtml . show
toSpan      = thespan . toHtml
reportStyle = style $ toHtml "td { border: 1px solid #eee; }"

