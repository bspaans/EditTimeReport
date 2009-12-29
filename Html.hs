--module Html where

import Printers
import Text.XHtml
import System ( getArgs )

import Control.Applicative




html :: SIndexedReport -> Html
html = foldIR htmlAlgebra


htmlFromFile :: FilePath -> IO StatOptions -> IO Html
htmlFromFile f = fmap html . indexFromFile f


htmlAlgebra :: SIndexedAlgebra Html Html Html Html
htmlAlgebra = (concatHtml, y, m, d)
  where y (year, months) = h3 (o year) 
                        +++ concatHtml months
        m (month, days) = h4 (toHtml $ getMonth month)
                        +++ table (concatHtml days)
        d (day, stats) = tr (td (o day)
                             +++ td (table $ header +++ map showEntry stats))
        header = tr (concatHtml $ map (th . stringToHtml) headers)
        o = toHtml . show


showEntry :: EditStats -> Html
showEntry s = tr (concatHtml $ map (\(f, c) -> td (f s) ! [theclass c]) funcs)
  where funcs = [ (showTimes                    , "time"     ), 
                  (toHtml . fileName            , "filename" ),
                  (showExtension                , "extension"),
                  (showSub language "matchlang" , "language" ),
                  (showSub project  "matchproj" , "project"  ) ]


showTimes s = if diff == (0, 0, 0) then noHtml
                   else thespan (stringToHtml (showTime diff)) 
   where diff = editTime s


showExtension = maybe noHtml (\x -> thespan (toHtml (concat [" [", x, "] "]))) . extInformation

showSub f c st = case f st of  
                   Nothing -> noHtml 
                   Just x -> thespan (stringToHtml (concat [" {", fst x, ":", snd x, "} "])) ! [theclass c]





instance Printer Html where
  printReport o a = prettyHtml $ a +++ reportStyle 









--- DEBUG DEBUG DEBUG DEBUG --
--
fileToHTMLReport :: FilePath -> IO String
fileToHTMLReport f = printReport defaultPO <$> htmlFromFile f defaultIOSO

reportStyle = style $ stringToHtml "td { border: 1px solid #eee; }"

main = do args <- getArgs
          if length args < 1 
            then usage
            else if length args == 1 then fileToHTMLReport (head args) >>= putStrLn
                                     else fileToHTMLReport (head args) >>= writeFile (args !! 1)
usage = do putStrLn "Report generator"
           putStrLn "Copyright 2009, Bart Spaans"
           putStrLn "\n  Usage: genreport LOG [OUTPUT]"
