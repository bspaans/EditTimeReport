module Main where


-- Currently only implemented HTML reports
--
import HtmlPrinter
import Printers
import Query
import System ( getArgs )


-- TODO getOpt here
--
main = do args <- getArgs
          po <- return defaultPO
          if length args < 1 
            then usage
            else do so <- askStatOptions
                    if length args == 1 then htmlFromFile (head args) po so >>= putStrLn
                                        else if length args == 2 && args !! 1 /= "-i" then htmlFromFile (head args) po so >>= writeFile (args !! 1)
								else do s <- statsFromFile (head args) so
								        interactiveQueries s



usage = do putStrLn "Report generator"
           putStrLn "Copyright 2009, Bart Spaans"
           putStrLn "\n  Usage: report LOG [OUTPUT]\n"
