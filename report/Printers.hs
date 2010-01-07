module Printers ( PrintOptions(..), POption (..)
                , isSet, defaultPO
                , touched
                , months, headers     -- String data
                , getMonth, getDow 
                , showTimeE, showTime -- Time strings
                , brackets, braced, showSub, showLanguage, showProject
                , showExtension, printString, printHtml, printCSV
                ) where

import Stats
import qualified Text.Html as H
import qualified Text.XHtml as X
import Text.Printf
import qualified Text.CSV as CSV
import Data.Monoid
import Control.Applicative
import Control.Arrow hiding ((+++))


data PrintOptions = PO [ POption ]
data POption      = StyleSheet String
                  | GenerateHeader
                  deriving (Eq, Show)

instance Monoid PrintOptions where
  mappend (PO a) (PO b) = PO $ mappend a b
  mempty = PO []


isSet op (PO opts) = elem op opts

defaultPO = PO [StyleSheet "td { border: 1px solid #eee; }" -- Should actually be a file though
            ]


touched = map (file . edit)

months :: [String]
headers :: [String]
days :: [String]
getMonth :: Int -> String
getDow :: Int -> String
months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
getMonth n = months !! (min 12 (max 1 n) - 1)

days = ["Sunday", "Monday", "Tuesday", "Wednesday",
        "Thursday", "Friday", "Saturday"]
getDow n = days !! min 6 (max 0 n)

headers = ["Time", "File", "Extension", "Language", "Project"]


showTimeE :: Edit -> String
showTimeE (Edit _ _ _ h m s _ _ _) = showTime (h, m, s)

showTime :: (Int, Int, Int) -> String
showTime (h, m, s) = concat [f h, ":", f m, ":", f s]
  where f = printf "%02d"

enclose :: String -> String -> String -> String
enclose a c b = concat [a, b, c]

brackets = enclose "[" "]" 
braced   = enclose "{" "}" 


showSub :: (Description, String) -> String
showSub (r, s) = braced $ r ++ ":" ++ s

showExtension' :: String -> EditStats -> String
showLanguage' n = showLanguage n . language
showProject' n = showProject n . project
showExtension' n = showExtension n . extInformation

showLanguage  n = maybe n showSub 
showProject   n = maybe n showSub 
showExtension n = maybe n brackets



-- StatsTree to Plain Text
--
printString :: StatsTree -> String
printString (Root [] _ _)      = "No matches"
printString (Root ns _ t)      = concatMap (tts' 1) ns ++ "\n"
  where tts' lvl (Leaf time)   = printf "%-10s" (showTime time)
        tts' lvl (Node _ s tr) = '\n' : replicate (lvl * 4) ' ' 
                              ++ printf "%-70s" s 
                              ++ concatMap (tts' (lvl + 1)) tr



-- StatsTree to Html table
--
printHtml :: StatsTree -> String
printHtml = H.prettyHtml . foldTree (root, node, leaf)
  where root ns h t = H.h2 (H.toHtml t) H.+++ H.table (headers h H.+++ makeTable ns) H.+++ H.hr
        headers h   = H.tr (H.concatHtml (map (H.th . H.toHtml) h))
        makeTable ns = H.concatHtml (concatMap (map H.tr) ns)
        node cspan  = concatMap . map  . (H.+++) . H.td . H.toHtml
        leaf        = pure . H.td . H.toHtml . showTime


-- StatsTree to XHtml table
--
printXHtml :: StatsTree -> String
printXHtml = X.prettyHtml . foldTree (root, node, leaf)
  where root ns h t = X.h2 (X.toHtml t) X.+++ X.table (headers h X.+++ makeTable ns) X.+++ X.hr
        headers h   = X.tr (X.concatHtml (map (X.th . X.toHtml) h))
        makeTable ns = X.concatHtml (concatMap (map X.tr) ns)
        node cspan  = concatMap . map  . (X.+++) . X.td . X.toHtml
        leaf        = pure . X.td . X.toHtml . showTime


-- StatsTree to CSV 
--
printCSV :: StatsTree -> String
printCSV = CSV.printCSV . foldTree (root, node, leaf)
  where root ns h _ = h : concat ns
        node _      = concatMap . map . (:)
        leaf        = pure . pure . showTime
