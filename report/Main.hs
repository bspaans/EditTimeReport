module Main where


-- Currently only implemented HTML reports
--
import HtmlPrinter
import Printers
import System ( getArgs )


-- TODO getOpt here
--
main = do args <- getArgs
          po <- return defaultPO
          so <- askStatOptions
          if length args < 1 
            then usage
            else if length args == 1 then htmlFromFile (head args) po so >>= putStrLn
                                     else htmlFromFile (head args) po so >>= writeFile (args !! 1)



usage = do putStrLn "Report generator"
           putStrLn "Copyright 2009, Bart Spaans"
           putStrLn "\n  Usage: genreport LOG [OUTPUT]"
