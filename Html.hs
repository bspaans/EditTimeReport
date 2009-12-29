--module Html where

import Report
import Text.XHtml
import Text.Printf
import System ( getArgs )

import Control.Applicative

html :: SIndexedReport -> Html
html = foldIR htmlAlgebra


htmlFromFile :: FilePath -> IO StatOptions -> IO Html
htmlFromFile f = fmap html . indexFromFile f

htmlAlgebra :: SIndexedAlgebra Html Html Html Html
htmlAlgebra = (concatHtml, y, m, d)
  where y (year, months) = h3 (toHtml . show $ year) 
                        +++ concatHtml months
        m (month, days) = h4 (toHtml . show $ month)
                        +++ table (concatHtml days)
        d (day, stats) = table (header +++ map showEntry stats)
        header = tr (concatHtml . map ((! [thestyle "text-align: left; background: #222; color: #eee"] ) . th . stringToHtml) $ ["Time", "File", "Extension", "Language", "Project"])

touched = map (file . edit)


showEntry s = tr (td (showTimes s) ! [width "80px"]
           +++ td (stringToHtml (" " ++ fileName s)) ! [width "400px"]
           +++ td (showExtension s) ! [width "150px"]
           +++ td (showSub s language "#00d") ! [width "150px"]
           +++ td (showSub s project "#dd0"))


showTimes s = if diff == (0, 0, 0) then noHtml
                   else thespan (stringToHtml (showTime diff)) ! [thestyle "color:#00dd00;"]
   where diff = editTime s

showExtension st = case extInformation st of { Nothing -> noHtml ; Just x -> (thespan (stringToHtml (concat [" [", x, "] "]))) ! [ thestyle "color:#dd00dd"] }
showSub st f c = case f st of  
                   Nothing -> noHtml 
                   Just x -> (thespan (stringToHtml (concat [" {", fst x, ":", snd x, "} "]))) ! [ thestyle ("color:" ++ c)]

showTimeE (Edit _ _ _ h m s _ _ _) = showTime (h, m, s)
showTime (h, m, s) = concat [f h, ":", f m, ":", f s]
  where f = printf "%02d"


--- DEBUG DEBUG DEBUG DEBUG --
--
fileToHTMLReport :: FilePath -> IO String
fileToHTMLReport f = prettyHtml . (+++ reportStyle) <$> htmlFromFile f defaultIOSO

reportStyle = style $ stringToHtml "td { border: 1px solid #eee; }"

main = do args <- getArgs
          if length args < 1 
            then usage
            else if length args == 1 then fileToHTMLReport (head args) >>= putStrLn
                                     else fileToHTMLReport (head args) >>= writeFile (args !! 1)
usage = do putStrLn "Report generator"
           putStrLn "Copyright 2009, Bart Spaans"
           putStrLn "\n  Usage: genreport LOG [OUTPUT]"
