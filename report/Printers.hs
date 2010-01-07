module Printers ( PrintOptions(..), POption (..)
                , isSet, defaultPO
                , touched
                , months, headers     -- String data
                , getMonth, getDow 
                , showTimeE, showTime -- Time strings
                , brackets, braced, showSub, showLanguage, showProject
                , showExtension, treeToString, treeToHtml, treeToCSV
                ) where

import Stats
import Text.XHtml hiding (header)
import Text.Printf
import Text.CSV
import Data.Monoid
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
getMonth n = months !! ((min 12 (max 1 n)) - 1)

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
treeToString :: StatsTree -> String
treeToString (Root [] _ _) = "No matches"
treeToString (Root ns _ t) = concatMap (tts' 1) ns ++ "\n"
  where tts' lvl (Leaf time) = printf "%-10s" (showTime time)
        tts' lvl (Node _ s tr) = '\n' : replicate (lvl * 4) ' ' 
                              ++ printf "%-70s" s 
                              ++ concatMap (tts' (lvl + 1)) tr



-- StatsTree to Html table
--
treeToHtml :: StatsTree -> String
treeToHtml = prettyHtml . foldTree (root, node, leaf)
  where root ns h t = table (tr (concatHtml (map (th . toHtml) h)) +++ (concatHtml . map tr $ ns))
        node cspan s ns = (td (toHtml s) ! [colspan cspan]) +++ table (concatHtml (map tr ns))
        leaf = td . toHtml . showTime



-- StatsTree to CSV 
--
treeToCSV :: StatsTree -> String
treeToCSV = printCSV . foldTree (root, node, leaf)
  where root :: [[[String]]] -> [String] -> String -> [[String]]
        root ns h t = h : concat ns
        node :: Int -> String -> [[[String]]] -> [[String]]
        node _ s ns = concatMap (map (s :)) ns
        leaf :: Time -> [[String]]
        leaf t = [[showTime t]]
