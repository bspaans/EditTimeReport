{-# LANGUAGE FlexibleInstances #-}

module Printers ( Printer(printReport), PrintOptions
                 , defaultPO
                 , touched
                 , months, headers     -- String data
                 , getMonth
                 , showTimeE, showTime -- Time strings
                 , module Report
                 ) where
import Report
import Text.Printf


-- Stub
data PrintOptions = P Int

defaultPO = P 12


class Show a => Printer a where 
  printReport :: PrintOptions -> a -> String
  printReport = const show



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
