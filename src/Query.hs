{-# LANGUAGE Rank2Types, FlexibleContexts #-}
-- | This module can take a parsed query, 
-- convert it into something it can use and 
-- execute it; resulting in a StatsTree, which 
-- can be printed by one of the Printers.
--
module Query ( 
               -- * Commands
               Commands, Queries, Query, SubQuery, 
               View, Constraint, Group, Env,
               -- * Execute Queries 
               interactiveQueries, 
               commandsFromFile, commandsFromFiles,
               makeTree, QueryAST.E(..),
               emptyEnv, execute, execute' 
             , module Printers 
             ) where


import Stats
import Printers
import QueryParser
import QueryAST
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

type Commands   = (Queries, Env)
type ExecResult = ([StatsTree], Env)
type Queries    = [Query]
type Env        = D.Map String Query

-- | Each query is represented by sub queries, 
-- each of which adds a new level to the tree.
--
type Query      = ([SubQuery], Maybe String)
type SubQuery   = (Header, View, Constraint, Group)
type Constraint = Stats -> (Stats, Stats)
type View       = EditStats -> String
type Group      = Stats -> [Stats]


-- | Read commands from files in a given environment.
--
commandsFromFiles :: Env -> [FilePath] -> ET IO Commands
commandsFromFiles env []     = return ([], env)
commandsFromFiles env (f:fp) = commandsFromFile env f 
                           >>= \(q,e) -> commandsFromFiles e fp 
                           >>= \(q2,e) -> return (q ++ q2, e)

-- | Read commands from a file in a given environment.
--
commandsFromFile :: Env -> FilePath -> ET IO Commands
commandsFromFile env = ET . fmap (>>= fromQCommands env) . parseFile


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


-- A Constraint separates Stats matching 
-- a predicate from the ones that don't
-- (P(s), ¬P(s))
--
makeConstraint :: Pred EditStats -> Constraint
makeConstraint p = con ([], [])
  where con (yes, no) [] = (yes, no)
        con (yes, no) (s:st) = con n st
          where n = if p s then (s : yes, no) else (yes, s:no)




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


-- Convert AST to Query
-- We have to transform the parsing result (QCommands) into 
-- SubQueries (ie. a view function, a constraint and a 
-- group function) and an Environment.
--
fromQCommands    :: Env -> QCommands -> E Commands
fromQCommand     :: Env -> QCommand -> E (Either Query Env)
fromQQuery       :: Env -> QQuery -> E Query
fromQSubQuery    :: Env -> QSubQuery -> E [SubQuery]
fromQIndex       :: QIndex -> (EditStats -> String)
addGrouping      :: Ord a => Bool -> (EditStats -> a) -> Group

fromQCommands env qs = fromQCommands' env [] qs
  where fromQCommands' env res []     = Ok (res, env)
        fromQCommands' env res (q:qs) = fromQCommand env q >>= f
          where f (Left qr) = fromQCommands' env (qr : res) qs 
                f (Right e) = fromQCommands' newEnv res qs
                   where newEnv = D.union e env
 

fromQCommand env (Left q)  = Left <$> fromQQuery env q
fromQCommand env (Right a) = Right <$> i a
  where i (QAssign s q) = flip (D.insert s) env <$> fromQQuery env q


fromQQuery env ([], s)    = Ok ([], s)
fromQQuery env ((c:cs),s) = fromQSubQuery env c >>= 
                             (\a -> flip (,) s . (a++) . fst <$> fromQQuery env (cs,s))




-- Sub queries
--
fromQSubQuery _ q@(QSubQuery _ Ext   _ _ _ _) = makeQuery q (takeExtension . fileName)
fromQSubQuery _ q@(QSubQuery _ Lang  _ _ _ _) = makeQuery q language
fromQSubQuery _ q@(QSubQuery _ Proj  _ _ _ _) = makeQuery q project
fromQSubQuery _ q@(QSubQuery _ Path  _ _ _ _) = makeQuery q fileName
fromQSubQuery _ q@(QSubQuery _ File  _ _ _ _) = makeQuery q (takeFileName . fileName)
fromQSubQuery _ q@(QSubQuery _ Dir   _ _ _ _) = makeQuery q (takeDirectory . fileName)
fromQSubQuery _ q@(QSubQuery _ Year  _ _ _ _) = makeQuery q (year . edit)
fromQSubQuery _ q@(QSubQuery _ Month _ _ _ _) = makeQuery q (month . edit)
fromQSubQuery _ q@(QSubQuery _ Week  _ _ _ _) = makeQuery q (week . edit)
fromQSubQuery _ q@(QSubQuery _ Day   _ _ _ _) = makeQuery q (day . edit)
fromQSubQuery _ q@(QSubQuery _ Dow   _ _ _ _) = makeQuery q (dow . edit)
fromQSubQuery _ q@(QSubQuery _ Doy   _ _ _ _) = makeQuery q (doy . edit)
fromQSubQuery env (QCall s)                 = case D.lookup s env of 
                                                Just (q,s) -> Ok q
                                                Nothing -> Failed $ "Unknown definition `" ++ s ++ "'"

makeQuery (QSubQuery gr t c h o l) f  = Ok [(fromQAs h t, view, constraints, grouping)]
   where view = fromQIndex t
         constraints = fromQConstraints t c
         grouping = fromQLimit l . fromQOrder o . addGrouping gr f


fromQAs (As s) _     = s
fromQAs _      Ext   = "Extension"
fromQAs _      Lang  = "Language"
fromQAs _      Proj  = "Project"
fromQAs _      Path  = "File Path"
fromQAs _      File  = "File Name"
fromQAs _      Dir   = "Directory"
fromQAs _      Year  = "Year"
fromQAs _      Month = "Month"
fromQAs _      Week  = "Week"
fromQAs _      Day   = "Day"
fromQAs _      Dow   = "Day of the Week"
fromQAs _      Doy   = "Day of the Year"

-- Limiting and ordering is done before passing 
-- the grouped Stats on to the constraint function
--
fromQOrder :: QOrder -> [Stats] -> [Stats]
fromQOrder NoOrder = id
fromQOrder Asc     = sortBy (compare `on` sumTime)
fromQOrder Desc    = sortBy (flip compare `on` sumTime)


fromQLimit :: QLimit -> [Stats] -> [Stats]
fromQLimit NoLimit   = id
fromQLimit (Limit i) = take i 


-- The view function
--
fromQIndex Ext   = takeExtension . fileName
fromQIndex Lang  = maybe "Unknown language" snd . language
fromQIndex Proj  = maybe "Unknown project" snd . project
fromQIndex Path  = fileName
fromQIndex File  = takeFileName . fileName 
fromQIndex Dir   = takeDirectory . fileName 
fromQIndex Year  = show . year . edit
fromQIndex Month = getMonth . month . edit
fromQIndex Week  = show . week . edit
fromQIndex Day   = show . day . edit
fromQIndex Dow   = getDow . dow . edit
fromQIndex Doy   = show . doy . edit



-- The grouping function
--
addGrouping False _ = map (:[]) -- dontGroup
addGrouping True  f = groupWith f . sortBy (compare `on` f)


addToGrouping [] _ = error "Empty query, can't add to grouping"
addToGrouping cs f = init cs ++ [add (last cs)]
  where add (v, cs, gr) = (v, cs, f . gr)



-- QConstraints to Constraints
-- Individual QConstraints are first converted to predicates
-- and get turned into one Constraint using makeConstraint.
--
fromQConstraints :: QIndex -> [QConstraint] -> Constraint
makePred         :: QIndex -> QConstraint -> Pred EditStats


fromQConstraints i qc = makeConstraint $ foldr f (const True) preds
  where f a b p       = a p && b p 
        preds         = map (makePred i) qc

makePred i (QC Year  op e) = numericalC op year e
makePred i (QC Month op e) = numStringC op e month getMonth
makePred i (QC Week  op e) = numericalC op week e
makePred i (QC Day   op e) = numericalC op day e
makePred i (QC Doy   op e) = numericalC op doy e
makePred i (QC Dow   op e) = numStringC op e dow getDow
makePred i (QC ind   op e) = fromQOper op (fromQExpr e) . fromQIndex ind
makePred i (QCOE     op e) = makePred i (QC i op e)
makePred i (QCE         e) = makePred i (QC i QE e)


-- helper functions; to do the conversion
stringC op s f    = fromQOper op (map toUpper s) . map toUpper . f . edit
numericalC op g e = fromQOper op (fromQExpr e) . show . g . edit
maybeC op g h e   = maybe False (fromQOper op (fromQExpr e) . h) . g

numStringC op (QInt i) num _      = fromQOper op (show i) . show . num . edit
numStringC op (QString s) num str = stringC op s (str . num)


-- Expressions 
--
fromQExpr :: QExpr -> String
fromQExpr (QInt i) = show i
fromQExpr (QString s) = s


-- Operators are written flipped around to make it easier 
-- to write in a point-free style (see above)
--
fromQOper :: QOper -> (String -> String -> Bool)
fromQOper QL  = (>)  
fromQOper QG  = (<)  
fromQOper QLE = (>=) 
fromQOper QGE = (<=) 
fromQOper QE  = (==)
fromQOper QNE = (/=)
fromQOper QREG = \m s -> isJust $ matchRegex (mkRegex m) s

