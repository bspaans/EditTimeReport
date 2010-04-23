{-# LANGUAGE Rank2Types, FlexibleContexts #-}
-- | This module can take a parsed query, 
-- convert it into something it can use and 
-- execute it; resulting in a StatsTree, which 
-- can be printed by one of the Printers.
--
module Query ( -- * Execute Queries 
               interactiveQueries, 
               commandsFromFile, commandsFromFiles,
               makeTree,
               emptyEnv, execute, execute' 
             , module Printers 
             , module QueryAST
             , module ASTtoQuery
             ) where


import Stats
import Printers
import QueryParser
import QueryAST
import ASTtoQuery
import Char
import Data.List
import Data.Function
import qualified Data.Map as D
import Maybe
import Control.Applicative
import System.Console.Editline.Readline
import System.IO
import System.Directory
import System.FilePath
import Text.Printf
import Text.Regex
import Control.Monad

import System.CPUTime


-- | Read commands from files in a given environment.
--
commandsFromFiles :: Env -> [FilePath] -> ET IO Commands
commandsFromFiles env []     = return ([], env)
commandsFromFiles env (f:fp) = commandsFromFile env f 
                           >>= \(q ,e ) -> commandsFromFiles e fp 
                           >>= \(q2,e') -> return (q ++ q2, e')

-- | Read commands from a file in a given environment.
--
commandsFromFile :: Env -> FilePath -> ET IO Commands
commandsFromFile env f = parseFile f >>= ET . return . fromQCommands env 


-- | This function executes Commands in a given environment
-- and then uses the Printers defined in the PrintOptions 
-- to return a String representation of the StatsTree.
--
execute :: Env -> PrintOptions -> E Commands -> Stats -> String
execute env po q st = case tr of 
                        Ok c -> printTree po c
                        Failed f -> error f 
  where tr = fst . executeCommands st <$> q
 
execute' env po q st = execute env po (mapM parseCommands q >>= fromQCommands env . concat) st

emptyEnv :: Env
emptyEnv = D.fromList []




executeCommands :: Stats -> Commands -> ExecResult
executeCommands st (q, env) = (map (flip makeTree st) q, env)

-- | Executing a Query: 
-- A Query works on Stats, which is a flat list of EditStats.
-- For our results to be meaningful we need to 
-- create a StatsTree, where each level represents another
-- subquery. Executing a query can thus be seen 
-- as building a tree from a list. 
--
makeTree        :: Query -> Stats -> StatsTree
makeTree ([],t) s = Root [] [] (fromMaybe "" t) (0,0,0)
makeTree (q, t) s = Root nodes headers title time'
  where (nodes, time') = makeTree' s q (0,0,0)
        headers = map (\(h,_,_,_) -> h) q ++ ["Edit Time"]
        title = fromMaybe (concat . intersperse " / " $ reverse headers) t


makeTree'       :: Stats -> [SubQuery] -> Time -> ([StatsTree], Time)
makeTree' s []              t = ([Leaf t], t)
makeTree' s ((_,v,c, g):cs) t = pruneNodeTrees nodes
  where (yes, no) = c s
        nodes = map (\gr -> makeNode (v . head $ gr) gr cs) (g yes)


-- Pruning is pretty easy because we need to remove 
-- the trees that don't have any leafs in them, 
-- and we already keep that information in the Nodes.
--
pruneNodeTrees :: [StatsTree] -> ([StatsTree], Time)
pruneNodeTrees tr = (trees, sumNodeTime trees)
  where f (Node i _ _ _) = i
        f _              = 1
        trees            = filter ((/=0) . f) tr

sumNodeTime :: [StatsTree] -> Time 
sumNodeTime [] = (0,0,0)
sumNodeTime ((Leaf _):cs) = sumNodeTime cs
sumNodeTime ((Node _ ti _ _):cs) = fromSeconds (toSeconds ti + (toSeconds . sumNodeTime $ cs))


-- A Node is made of an Int describing the number of leafs,
-- a string that is used for printing, and of course its children.
--
makeNode :: String -> Stats -> [SubQuery] -> StatsTree
makeNode s yes cs = Node (n tr) time' s tr
  where (tr, time') = makeTree' yes cs time
        time = sumTime yes
        n [] = 0       -- Count children
        n ((Node i _ _ _):cs) = i + n cs
        n ((Leaf _):cs) = 1 + n cs
       

-- A helper function that builds a StatsTree out 
-- of a String in the Query language.
--
treeFromQuery :: String -> Env -> Stats -> E ExecResult
treeFromQuery s env st = executeCommands st <$> (parseCommands s >>= fromQCommands env)



-- | Interactive Query prompt using editline
--
interactiveQueries :: PrintOptions -> Stats -> Env -> IO()
promptStart :: IO Bool

type Action = Stats -> Env -> PrintOptions -> IO()

printerDict = [("csv", Csv), ("html", Html), ("text", Text), ("xhtml", XHtml)]


makePrintAction (s, pr) = [('-' : s, (m un , "Disable " ++ s ++ " printer"))
                         , ('+' : s, (m se , "Enable "  ++ s ++ " printer"))]
  where un = unSet (PrinterF pr)
        se = set   (PrinterF pr)
        m f stats env = repl stats env . f



replCommands = concatMap makePrintAction printerDict
            ++ [("exit", (\_ _ _ -> onExit, "Exit Program"))
              , ("help", (\s e p -> printHelp >> repl s e p, "Output help"))]


printHelp = putStrLn $ '\n' : unlines (sort (map h replCommands))
  where h (c, (_, d)) = printf "   %-8s  %s" c d


interactiveQueries po stats env = promptStart >> repl stats env po
repl stats env po = do
         setCompletionEntryFunction (Just $ qCompleter env)
         maybeLine <- readline "> " 
         case maybeLine of
           Nothing      -> onExit
           Just ""      -> repl stats env po
           Just s       -> do addHistory s
                              case lookup s replCommands of 
                                Just (c, _) -> c stats env po
                                Nothing -> eval stats env po s

eval stats env po s = do 
  t1 <- getCPUTime
  case treeFromQuery s env stats of
    Ok (st, e) -> do let fromPico p = fromInteger p * 1e-12 :: Float
                         pr' s p = printf s (fromPico p)
                     putStrLn $ printTree po st
                     t2 <- getCPUTime 
                     when (not (ShowExecTime False `isSet` po))
                       (putStrLn (pr' "\nQuery executed in %.5f" (t2 - t1)
                               ++ pr' " (+/-%.5f) seconds." cpuTimePrecision))
                     repl stats e po
    Failed e -> putStrLn e >> repl stats env po
  



promptStart = do putStrLn (unlines ["Time Report 1.0a, interactive session"
                                   , "Copyright 2009-2010, Bart Spaans"
                                   , "Type \"help\" for more information"])
                 hSetBuffering stdout NoBuffering  -- remove LineBuffering from stdout
                 historyLocation >>= readHistory

onExit = putStr "\n" >> historyLocation >>= writeHistory >> return ()

historyLocation :: IO FilePath
historyLocation = (</> ".report_history") <$> getUserDocumentsDirectory

qCompleter :: Env -> String -> IO [String]
qCompleter env s = return (filter (startsWith s) known)
  where known = ["extension", "language", "project", "filename"
               , "path", "directory", "week"
               , "year", "month", "day", "dow", "doy", "limit"
               , "asc", "desc"] ++ D.keys env ++ map fst replCommands


