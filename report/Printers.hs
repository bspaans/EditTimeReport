{-# LANGUAGE FlexibleInstances #-}

module Printers ( Printer(printReport), PrintOptions
                 , defaultPO
                 , touched
                 , months, headers     -- String data
                 , getMonth
                 , showTimeE, showTime -- Time strings
                 , brackets, braced, showExtension
                 , module Report
                 ) where
import Report
import Text.Printf


data PrintOptions = PO [ POption ]
data POption      = StyleSheet String
                  | GenerateHeader deriving (Eq, Show)

defaultPO = P 12


class Printer a where 
  printReport :: PrintOptions -> a -> String


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



showExtension :: EditStats -> String
showExtension = maybe "" brackets . extInformation
