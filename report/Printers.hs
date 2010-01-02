{-# LANGUAGE FlexibleInstances #-}

module Printers ( Printer(printReport)
                 , PrintOptions(..), POption (..)
                 , isSet, defaultPO
                 , touched
                 , months, headers     -- String data
                 , getMonth, getDow 
                 , showTimeE, showTime -- Time strings
                 , brackets, braced, showSub, showLanguage, showProject
                 , showExtension, treeToString
                 , module Report
                 ) where

import Report
import Text.Printf
import Data.Monoid
import Control.Arrow


data PrintOptions = PO [ POption ]
data POption      = StyleSheet String
                  | GenerateHeader
                  | PrintSIndexed
                  | PrintExtensionTable
                  | PrintLanguageTable
                  | PrintProjectTable
                  | PrintFilenameTable
                  | PrintYearTable
                  | PrintMonthTable
                  | PrintDayTable
                  | PrintDayofWeekTable
                  | SortedAsc
                  | SortedDesc
                  deriving (Eq, Show)

instance Monoid PrintOptions where
  mappend (PO a) (PO b) = PO $ mappend a b
  mempty = PO []


isSet op (PO opts) = elem op opts

defaultPO = PO [StyleSheet "td { border: 1px solid #eee; }" -- Should actually be a file though
              , PrintSIndexed, PrintExtensionTable
              , PrintLanguageTable, PrintProjectTable 
              , PrintFilenameTable, PrintMonthTable, PrintDayofWeekTable]

printAllPO = PO [ PrintSIndexed, PrintExtensionTable
              , PrintLanguageTable, PrintProjectTable 
              , PrintFilenameTable, PrintYearTable
              , PrintMonthTable, PrintDayTable 
              , PrintDayofWeekTable]


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


-- TimeTable a to TimeTable String
--
tableS f t = map (first (f)) . t
tableExtensionsS = tableS (showExtension "NONE") tableExtensions
tableLanguagesS  = tableS (showLanguage "NONE") tableLanguages
tableProjectsS   = tableS (showProject "NONE") tableProjects
tableFilenamesS  = tableFilenames
tableYearS       = tableS show tableYear
tableMonthS      = tableS show tableMonth
tableDayS        = tableS show tableDay
tableDayofWeekS  = tableS show tableDayofWeek




treeToString :: StatsTree -> String
treeToString (Root ns) = concatMap (tts' 1) ns ++ "\n"
  where tts' lvl (Leaf time) = showTime time
        tts' lvl (Node _ s tr) = '\n' : replicate (lvl * 4) ' ' 
                              ++ printf "%-70s" s 
                              ++ concatMap (tts' (lvl + 1)) tr

