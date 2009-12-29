module Main where


-- Currently only implemented HTML reports
--
import Html
import System ( getArgs )


-- TODO getOpt here
--
main = do args <- getArgs
          if length args < 1 
            then usage
            else if length args == 1 then fileToHTMLReport (head args) >>= putStrLn
                                     else fileToHTMLReport (head args) >>= writeFile (args !! 1)



usage = do putStrLn "Report generator"
           putStrLn "Copyright 2009, Bart Spaans"
           putStrLn "\n  Usage: genreport LOG [OUTPUT]"
