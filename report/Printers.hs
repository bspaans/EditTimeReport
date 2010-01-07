module Printers ( PrintOptions, POption(..)      -- Options
                , PrinterF(..), emptyPO          -- Options
                , showTime                       -- Time strings
                , getMonth, getDow               -- String converters
                , printTree, printCSV, printHtml -- Printers
                , printPlainText, printXHtml     -- Printers
                ) where

import Control.Applicative
import qualified Data.Set as S
import Stats
import Text.Printf
import qualified Text.CSV as C
import qualified Text.Html as H
import qualified Text.XHtml as X


type PrintOptions = S.Set POption
data POption      = PrinterF PrinterF
                  | Ident Int
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
getPrinters :: PrintOptions -> [StatsTree -> String]

isSet   = S.member 
set     = S.insert

getPrinters = map (printer . fromPrinters) . filter isPrinter . S.elems
  where printer Csv               = printCSV 
        printer Html              = printHtml 
        printer Text              = printPlainText 
        printer XHtml             = printXHtml 
        fromPrinters (PrinterF p) = p
        isPrinter (PrinterF _)    = True
        isPrinter _               = False


printTree :: StatsTree -> PrintOptions -> String
printTree st = concatMap ($st) . getPrinters 

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
printPlainText (Root [] _ _)   = "No matches"
printPlainText (Root ns _ t)   = concatMap (tts' 1) ns ++ "\n"
  where tts' lvl (Leaf time)   = printf "%-10s" (showTime time)
        tts' lvl (Node _ s tr) = '\n' : replicate (lvl * 4) ' ' 
                              ++ printf "%-70s" s 
                              ++ concatMap (tts' (lvl + 1)) tr



-- StatsTree to Html table
--
printHtml :: StatsTree -> String
printHtml = H.prettyHtml . foldTree (root, node, leaf)
  where root ns h t = H.h2 (H.toHtml t) H.+++ H.table (headers h H.+++ makeTable ns) H.+++ H.hr
        headers     = H.tr . H.concatHtml . map (H.th . H.toHtml) 
        makeTable   = H.concatHtml . concatMap (map H.tr) 
        node cspan  = concatMap . map  . (H.+++) . H.td . H.toHtml
        leaf        = pure . H.td . H.toHtml . showTime


-- StatsTree to XHtml table
--
printXHtml :: StatsTree -> String
printXHtml = X.prettyHtml . foldTree (root, node, leaf)
  where root ns h t = X.h2 (X.toHtml t) X.+++ X.table (headers h X.+++ makeTable ns) X.+++ X.hr
        headers     = X.tr . X.concatHtml . map (X.th . X.toHtml) 
        makeTable   = X.concatHtml . concatMap (map X.tr) 
        node cspan  = concatMap . map  . (X.+++) . X.td . X.toHtml
        leaf        = pure . X.td . X.toHtml . showTime


-- StatsTree to CSV 
--
printCSV :: StatsTree -> String
printCSV = C.printCSV . foldTree (root, node, leaf)
  where root ns h _ = h : concat ns
        node _      = concatMap . map . (:)
        leaf        = pure . pure . showTime
