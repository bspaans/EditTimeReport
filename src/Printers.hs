module Printers (
                 -- * Printers
                 printTree, printCSV, printHtml,
                 printPlainText, printXHtml, printStats,   

                 -- * Options
                 PrintOptions(..), PrinterF(..),
                 isSet, set, unSet, setPrinters,

                 -- * Time strings
                 showTime, showFmtTime,

                 -- * String converters
                 getMonth, getDow,
                ) where

import QueryAST 
import Stats

import Control.Applicative
import Data.Char
import qualified Data.Set as S
import qualified Text.CSV as C
import qualified Text.Html as H
import qualified Text.XHtml as X
import Text.Printf
import Text.Regex


data PrintOptions = PrintOptions { printers     :: S.Set PrinterF,
                                   showExecTime :: Bool,
                                   timeFmt      :: TimeFmt }

data PrinterF = Csv 
              | Html 
              | Text 
              | XHtml 
              deriving (Eq, Ord, Show)

type TimeFmt = String

defaultOptions :: PrintOptions
defaultOptions = PrintOptions { printers = S.fromList [Text],
                                showExecTime = True, 
                                timeFmt = "%0h:%0m:%0s" }


-- Handling Printers
--
isSet       :: PrinterF -> PrintOptions -> Bool
isSet p    = S.member p . printers 

set         :: PrinterF -> PrintOptions -> PrintOptions
set   p po = po { printers = S.insert p $ printers po }

unSet       :: PrinterF -> PrintOptions -> PrintOptions
unSet p po = po { printers = S.delete p $ printers po }



getPrinters :: PrintOptions -> [StatsTree -> String]
getPrinters po = map (flip printer $ timeFmt po) . S.elems $ printers po
  where printer Csv               = printCSV 
        printer Html              = printHtml 
        printer Text              = printPlainText 
        printer XHtml             = printXHtml 


parsePrinter :: String -> E PrinterF
parsePrinter "CSV"   = Ok Csv
parsePrinter "HTML"  = Ok Html
parsePrinter "TEXT"  = Ok Text
parsePrinter "XHTML" = Ok XHtml
parsePrinter s = Failed $ "No such format `" ++ s ++ "'"



setPrinters :: [String] -> E PrintOptions
setPrinters [] = Ok defaultOptions
setPrinters p  = case mapM (parsePrinter . map toUpper) p of 
                   Failed s -> Failed s
                   Ok fmt   -> Ok ( defaultOptions { printers = S.fromList fmt } )



-- | Selects the printers described in the PrintOptions.
--
printTree :: PrintOptions -> [StatsTree] -> String
printTree po st = if null p then "No printers selected." 
                            else p >>= (\p' -> concatMap (p'$) (reverse st))
                             where p = getPrinters po



-- String Conversions
--
getMonth :: Int -> String
getMonth n = months !! (min 12 (max 1 n) - 1)


months   :: [String]
months     = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

getDow   :: Int -> String
getDow n   = days !! min 6 (max 0 n)


days     :: [String]
days       = ["Sunday"  , "Monday", "Tuesday" , "Wednesday",
              "Thursday", "Friday", "Saturday"]

showTime :: Time -> String
showTime (h, m, s) = concat [f h, ":", f m, ":", f s]
  where f = printf "%02d"


showFmtTime :: TimeFmt -> Time -> String 
showFmtTime "%0h:%0m:%0s" t = showTime t
showFmtTime fmt (h, m, s)   = newS
  where repl = subRegex . mkRegex  . ('%':)
        newS = foldr (\(fmt', view) rep -> repl fmt' rep view) fmt dict
        dict = [("0h", printf "%02d" h), ("0m", printf "%02d" m),
                ("0s", printf "%02d" s), ("M" , show $ h * 60 + m),
                ("S" , show $ h * 3600 + m * 60 + s),
                ("h" , show h), ("m" , show m), ("s" , show s)]


printStats :: Stats -> String 
printStats = C.printCSV . (headers:) . map printEditStats 
  where headers = ["Year", "Month", "Week", "Day", "Hour", "Seconds", "Day of the Week",
                   "Day of the Year", "Filename", "EditTime: Hours", 
                   "EditTime: Minutes", "EditTime: Seconds",
                   "Filename (home replaced with ~)", "Project?", "Language?"]
  

printEditStats :: EditStats -> [String]
printEditStats edits = concat [printEdit (edit edits), time (editTime edits), [fileName edits,
                               printMaybe (project edits), printMaybe (language edits)]]
  where time (h, m, s) = [show h, show m, show s]
        printMaybe Nothing            = ""
        printMaybe (Just (desc, str)) = str 

printEdit :: Edit -> [String]
printEdit edit = [show $ year edit, show $ month edit, 
                  show $ week edit, show $ day edit, 
                  show $ hour edit, show $ minute edit, 
                  show $ seconds edit, show $ dow edit, show $ doy edit, file edit]


-- | StatsTree to Plain Text
--
printPlainText :: TimeFmt -> StatsTree -> String
printPlainText _ (Root [] _ _ _)   = "No matches"
printPlainText f (Root ns _ t ti)  = concatMap (tts' 1) ns ++ "\n\n   Total time: " 
                                ++ showFmtTime f ti ++ "\n" 
  where tts' lvl (Leaf time)       = printf "%10s" (showFmtTime f time)
        tts' lvl (Node _ _ s tr)   = '\n' : replicate (lvl * 4) ' ' 
                                ++ printf "%-70s" s 
                                ++ concatMap (tts' (lvl + 1)) tr


-- | StatsTree to Html table (no HTML header)
--
printHtml :: TimeFmt -> StatsTree -> String
printHtml fmt = H.prettyHtml . foldTree (root, node, leaf)
  where root ns h t ti = H.h2 (H.toHtml t) H.+++ H.table (headers h 
                         H.+++ makeTable ns H.+++ total) H.+++ H.hr
          where total  = H.tr(H.td (H.toHtml "TOTAL") H.! [H.colspan (length h - 1)] 
                         H.+++ H.td (H.toHtml . showFmtTime fmt $ ti))
        headers        = H.tr . H.concatHtml . map (H.th . H.toHtml) 
        makeTable      = H.concatHtml . concatMap (map H.tr) 
        node cspan t   = concatMap . map  . (H.+++) . H.td . H.toHtml
        leaf           = pure . H.td . H.toHtml . showFmtTime fmt

-- | StatsTree to XHtml table (includes HTML header)
--
printXHtml :: TimeFmt -> StatsTree -> String
printXHtml fmt = X.prettyHtml . foldTree (root, node, leaf)
  where root ns h t ti = X.h2 (X.toHtml t) X.+++ X.table (headers h 
                         X.+++ makeTable ns X.+++ total) X.+++ X.hr
          where total  = X.tr(X.td (X.toHtml "TOTAL") X.! [X.colspan (length h - 1)] 
                         X.+++ X.td (X.toHtml . showFmtTime fmt $ ti))
        headers        = X.tr . X.concatHtml . map (X.th . X.toHtml) 
        makeTable      = X.concatHtml . concatMap (map X.tr) 
        node cspan t   = concatMap . map  . (X.+++) . X.td . X.toHtml
        leaf           = pure . X.td . X.toHtml . showFmtTime fmt


-- | StatsTree to CSV String.
--
printCSV :: TimeFmt -> StatsTree -> String
printCSV fmt = C.printCSV . foldTree (root, node, leaf)
  where root ns h _ _ = h : concat ns
        node _ _      = concatMap . map . (:)
        leaf          = pure . pure . showFmtTime fmt
