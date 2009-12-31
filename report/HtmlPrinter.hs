module HtmlPrinter ( html, htmlFromFile   -- Convert SIndexedReports to Html table
                   , printReport          -- Making an instance of Printer
                   , fileToHTMLReport     -- Pretty printed Html
                   ) where

import Printers
import Text.XHtml hiding (header)
import Control.Applicative



-- Converting SIndexedReports to tables using its fold
--
html             :: SIndexedReport -> Html
htmlFromFile     :: FilePath -> IO StatOptions -> IO Html
htmlAlgebra      :: SIndexedAlgebra Html Html Html Html
--fileToHTMLReport :: FilePath -> IO String


html              = foldIR htmlAlgebra
htmlFromFile f    = fmap html . indexFromFile f
htmlAlgebra       = (concatHtml, showYear, showMonth, showDay)
fileToHTMLReport f = prettyHtml <$> p 
  where p = (printReport defaultPO <$> fromFile f defaultIOSO :: IO Html)


instance Printer Html where
  printReport opts cs = reportStyle +++ exts +++ lang +++ proj +++ name +++ ind
    where exts = test PrintExtensionTable htmlExtensionTable
          lang = test PrintLanguageTable htmlLanguageTable
          proj = test PrintProjectTable htmlProjectTable
          name = test PrintFilenameTable htmlFilenameTable 
          ind  = test PrintSIndexed  (html . report)
          test p f = if p `isSet` opts then f cs else noHtml


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
htmlTable          :: String -> Report (TimeTable a) -> (a -> Html) -> Report Html
htmlExtensionTable :: Report Html
htmlLanguageTable  :: Report Html
htmlProjectTable   :: Report Html
htmlFilenameTable  :: Report Html


htmlTable s r c u  = hr +++ p (table (h +++ (concatHtml . map row . r $ u)))
  where row (d, t) = tr (td (c d) +++ td (toHtml . showTime $ t))
        h          = tr (th  (toHtml s) +++ th (toHtml "Time"))


htmlExtensionTable = htmlTable "Extension" tableExtensions ext 
         where ext = toSpan . showExtension "NONE" 

htmlLanguageTable  = htmlTable "Language" tableLanguages lang 
       where lang  = toHtml . showLanguage "NONE"

htmlProjectTable   = htmlTable "Project" tableProjects proj
      where proj   = toHtml . showProject "NONE"

htmlFilenameTable  = htmlTable "Filename" tableFilenames toHtml



-- Helper functions
--
header      :: Html
toSpan      :: String -> Html
reportStyle :: Html

header      = tr (concatHtml $ map (th . toHtml) headers)
str         = toHtml . show
toSpan      = thespan . toHtml
reportStyle = style $ toHtml "td { border: 1px solid #eee; }"

