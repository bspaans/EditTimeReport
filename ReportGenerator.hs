import Stats
import Control.Applicative
import Text.XHtml
import Text.Printf
import System ( getArgs, getEnv )

showTimeE (Edit _ _ _ h m s _ _ _) = showTime (h, m, s)
showTime (h, m, s) = concat [f h, ":", f m, ":", f s]
  where f = printf "%02d"



months :: [String]
months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]


-- HTML Generation
touchedToHtml :: [FilePath] -> Html
touchedToHtml = unordList . map stringToHtml


dayHTML :: Edits -> Html
dayHTML es = table (concatHtml e) ! [thestyle "width: 100%"]
  where edits = fileGroup es
        d = map (\e -> (head e, last e)) edits
        
        e = header : map showEntry d
        header = tr (concatHtml . map ((! [thestyle "text-align: left; background: #222; color: #eee"] ) . th . stringToHtml) $ ["Time", "File", "Extension", "Language", "Project"])
        showEntry (h, l) = tr (td (showTimes s) ! [width "80px"]
                       +++ td (stringToHtml (" " ++ fileName s)) ! [width "400px"]
                       +++ td (showExtension s) ! [width "150px"]
                       +++ td (showSub s language "#00d") ! [width "150px"]
                       +++ td (showSub s project "#dd0"))
           where s = stats h l defaultSO
        showExtension st = case extInformation st of { Nothing -> noHtml ; Just x -> (thespan (stringToHtml (concat [" [", x, "] "]))) ! [ thestyle "color:#dd00dd"] }
        showSub st f c = case f st of  
                           Nothing -> noHtml 
                           Just x -> (thespan (stringToHtml (concat [" {", fst x, ":", snd x, "} "]))) ! [ thestyle ("color:" ++ c)]
        showTimes s = if diff == (0, 0, 0) then noHtml
                                           else thespan (stringToHtml (showTime diff)) ! [thestyle "color:#00dd00;"]
           where diff = editTime s


monthHTML :: [Edits] -> Html
monthHTML = table . concatHtml . map d ! [thestyle "width: 100%;"]
  where d e = tr ((td (stringToHtml . show . day $ head e)) +++ (td (dayHTML e)))

yearHTML :: [[Edits]] -> Html
yearHTML = table . concatHtml . map m ! [thestyle "width: 90%"]
  where m e = (h4 (stringToHtml (months !! ((month (head (head (e)))) - 1)))) +++ (monthHTML e)

hierarchyToTable :: CalendarE -> Html
hierarchyToTable = concatHtml . map y . fromCalendar
  where y e = (h3 . stringToHtml . show . year $ head . head . head $ e) +++ yearHTML e

fileToHTMLReport :: FilePath -> IO String
fileToHTMLReport f = prettyHtml . (+++ reportStyle) .  hierarchyToTable . calendarE <$> parseFile f

reportStyle = style $ stringToHtml "td { border: 1px solid #eee; }"

main = do args <- getArgs
          if length args < 1 
            then usage
            else if length args == 1 then fileToHTMLReport (head args) >>= putStrLn
                                     else fileToHTMLReport (head args) >>= writeFile (args !! 1)

usage = do putStrLn "Report generator"
           putStrLn "Copyright 2009, Bart Spaans"
           putStrLn "\n  Usage: genreport LOG [OUTPUT]"
