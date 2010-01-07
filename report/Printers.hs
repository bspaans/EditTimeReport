module Printers ( Printer(..)
                , PrintOptions(..), POption (..)
                , isSet, defaultPO
                , touched
                , months, headers     -- String data
                , getMonth, getDow 
                , showTimeE, showTime -- Time strings
                , brackets, braced, showSub, showLanguage, showProject
                , showExtension, treeToString
                ) where

import Stats
import Text.Printf
import Data.Monoid
import Control.Arrow


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


class Printer a where 
  printReport :: PrintOptions -> CalendarS -> a 


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


treeToString :: StatsTree -> String
treeToString (Root [] _ _) = "No matches"
treeToString (Root ns _ t) = concatMap (tts' 1) ns ++ "\n"
  where tts' lvl (Leaf time) = printf "%-10s" (showTime time)
        tts' lvl (Node _ s tr) = '\n' : replicate (lvl * 4) ' ' 
                              ++ printf "%-70s" s 
                              ++ concatMap (tts' (lvl + 1)) tr

