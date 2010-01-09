module Printers ( PrintOptions, POption(..)      -- Options
                , PrinterF(..), emptyPO          -- Options
                , isSet, set, unSet, setPrinters -- Options
                , showTime                       -- Time strings
                , getMonth, getDow               -- String converters
                , printTree, printCSV, printHtml -- Printers
                , printPlainText, printXHtml     -- Printers
                ) where

import QueryAST 
import Control.Applicative
import Data.Char
import qualified Data.Set as S
import Stats
import Text.Printf
import qualified Text.CSV as C
import qualified Text.Html as H
import qualified Text.XHtml as X


type PrintOptions = S.Set POption
data POption      = PrinterF !PrinterF
                  | Ident !Int
                  deriving (Eq, Ord, Show)
data PrinterF = Csv 
              | Html 
              | Text 
              | XHtml 
              deriving (Eq, Ord, Show)

emptyPO :: PrintOptions
emptyPO = S.fromList []


-- Handling PrintOptions
--
isSet       :: POption -> PrintOptions -> Bool
set         :: POption -> PrintOptions -> PrintOptions
unSet       :: POption -> PrintOptions -> PrintOptions
getPrinters :: PrintOptions -> [StatsTree -> String]
setPrinters :: [String] -> E PrintOptions

isSet   = S.member 
set     = S.insert
unSet   = S.delete

getPrinters = map (printer . fromPrinters) . filter isPrinter . S.elems
  where printer Csv               = printCSV 
        printer Html              = printHtml 
        printer Text              = printPlainText 
        printer XHtml             = printXHtml 
        fromPrinters (PrinterF p) = p
        isPrinter (PrinterF _)    = True
        isPrinter _               = False

parsePrinter "CSV"   = Ok Csv
parsePrinter "HTML"  = Ok Html
parsePrinter "TEXT"  = Ok Text
parsePrinter "XHTML" = Ok XHtml
parsePrinter s = Failed $ "No such format `" ++ s ++ "'"

setPrinters [] = Ok (S.fromList [PrinterF Text])
setPrinters p = S.fromList <$> f p
  where f = mapM (fmap PrinterF . parsePrinter . map toUpper) 


printTree :: PrintOptions -> [StatsTree] -> String
printTree po st = if null p then "No printers selected." 
                            else p >>= (\p' -> concatMap (p'$) (reverse st))
  where p = getPrinters po



-- String Conversions
--
getMonth :: Int -> String
getDow   :: Int -> String
showTime :: Time -> String
months   :: [String]
days     :: [String]

getMonth n = months !! (min 12 (max 1 n) - 1)
months     = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

getDow n   = days !! min 6 (max 0 n)
days       = ["Sunday"  , "Monday", "Tuesday" , "Wednesday",
              "Thursday", "Friday", "Saturday"]

showTime (h, m, s) = concat [f h, ":", f m, ":", f s]
  where f = printf "%02d"



-- StatsTree to Plain Text
--
printPlainText :: StatsTree -> String
printPlainText (Root [] _ _ _)   = "No matches"
printPlainText (Root ns _ t ti)   = concatMap (tts' 1) ns ++ "\n\n   Total time: " ++ showTime ti ++ "\n" 
  where tts' lvl (Leaf time)     = printf "%10s" (showTime time)
        tts' lvl (Node _ t s tr) = '\n' : replicate (lvl * 4) ' ' 
                                ++ printf "%-70s" s 
                                ++ concatMap (tts' (lvl + 1)) tr


-- StatsTree to Html table
--
printHtml :: StatsTree -> String
printHtml = H.prettyHtml . foldTree (root, node, leaf)
  where root ns h t ti = H.h2 (H.toHtml t) H.+++ H.table (headers h H.+++ makeTable ns H.+++ total) H.+++ H.hr
          where total  = H.tr(H.td (H.toHtml "TOTAL") H.! [H.colspan (length h - 1)] H.+++ H.td (H.toHtml . showTime $ ti))
        headers        = H.tr . H.concatHtml . map (H.th . H.toHtml) 
        makeTable      = H.concatHtml . concatMap (map H.tr) 
        node cspan t   = concatMap . map  . (H.+++) . H.td . H.toHtml
        leaf           = pure . H.td . H.toHtml . showTime

-- StatsTree to XHtml table
--
printXHtml :: StatsTree -> String
printXHtml = X.prettyHtml . foldTree (root, node, leaf)
  where root ns h t ti = X.h2 (X.toHtml t) X.+++ X.table (headers h X.+++ makeTable ns X.+++ total) X.+++ X.hr
          where total  = X.tr(X.td (X.toHtml "TOTAL") X.! [X.colspan (length h - 1)] X.+++ X.td (X.toHtml . showTime $ ti))
        headers        = X.tr . X.concatHtml . map (X.th . X.toHtml) 
        makeTable      = X.concatHtml . concatMap (map X.tr) 
        node cspan t   = concatMap . map  . (X.+++) . X.td . X.toHtml
        leaf           = pure . X.td . X.toHtml . showTime


-- StatsTree to CSV 
--
printCSV :: StatsTree -> String
printCSV = C.printCSV . foldTree (root, node, leaf)
  where root ns h _ _ = h : concat ns
        node _ _      = concatMap . map . (:)
        leaf          = pure . pure . showTime
