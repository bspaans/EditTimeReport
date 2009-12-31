{-# LANGUAGE FlexibleInstances #-}

module Printers ( Printer(printReport)
                 , PrintOptions(..), POption (..)
                 , isSet, defaultPO
                 , touched
                 , months, headers     -- String data
                 , getMonth
                 , showTimeE, showTime -- Time strings
                 , brackets, braced, showSub, showLanguage, showProject
                 , showExtension
                 , module Report
                 ) where

import Report
import Text.Printf
import Data.Monoid


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
                  deriving (Eq, Show)

instance Monoid PrintOptions where
  mappend (PO a) (PO b) = PO $ mappend a b
  mempty = PO []


isSet op (PO opts) = elem op opts

defaultPO = PO [StyleSheet "td { border: 1px solid #eee; }" -- Should actually be a file though
              , PrintSIndexed, PrintExtensionTable
              , PrintLanguageTable, PrintProjectTable 
              , PrintFilenameTable, PrintMonthTable, PrintDayofWeekTable]



class Printer a where 
  printReport :: PrintOptions -> CalendarS -> a 


touched = map (file . edit)

months :: [String]
headers :: [String]
months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
getMonth n = months !! (n - 1)
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

{-
showExtension' :: String -> EditStats -> String
showLanguage' n = showLanguage n . language
showProject' n = showProject n . project
showExtension' n = showExtension n . extInformation
-}

showLanguage  n = maybe n showSub 
showProject   n = maybe n showSub 
showExtension n = maybe n brackets
