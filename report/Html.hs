module Html ( html, htmlFromFile   -- Convert SIndexedReports to Html table
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
fileToHTMLReport :: FilePath -> IO String


html              = foldIR htmlAlgebra
htmlFromFile f    = fmap html . indexFromFile f
htmlAlgebra       = (concatHtml, showYear, showMonth, showDay)
fileToHTMLReport f = printReport defaultPO <$> htmlFromFile f defaultIOSO


instance Printer Html where
  printReport o a = prettyHtml $ a +++ reportStyle 



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


showExt          = maybe noHtml (toSpan . brackets) . extInformation
showLanguage' s   = maybe noHtml (showSub' language "matchlang" s) (language s)
showProject'  s   = maybe noHtml (showSub' project "matchproj"  s) (project s)
showSub' f c s   = const $ toSpan (showSub f s) ! [theclass c]


-- Tables
--

htmlTable :: Report (TimeTable a) -> (a -> Html) -> Report Html
htmlTable r c = concatHtml . map row . r
  where row (dat, time) = tr (td (c dat) +++ td (toHtml . showTime $ time))

htmlExtensionTable :: Report Html
htmlExtensionTable = htmlTable tableExtensions ext 
  where ext = (maybe (toSpan "NONE") (toSpan . brackets))


-- Helper functions
--
header      :: Html
toSpan      :: String -> Html
reportStyle :: Html

header      = tr (concatHtml $ map (th . toHtml) headers)
str         = toHtml . show
toSpan      = thespan . toHtml
reportStyle = style $ toHtml "td { border: 1px solid #eee; }"

