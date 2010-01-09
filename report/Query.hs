module Query ( makeTree, interactiveQueries, E(..)
             , emptyEnv, execute, execute' 
             , commandsFromFile, commandsFromFiles
             , module Printers 
             ) where

{-
 
 Examples of the embedded Query Language:

   1.   group language * group extension
   2.   language * extension   # same as no 1; grouping is default
   3.   language * nogroup extension    
   4.   nogroup language * nogroup extension
   5.   language limit 5
   6.   language ascending
   7.   language (language == "Haskell")
   8.   language (=="Haskell") # same as no 7; index is default 
   9.   language ("Haskell")   # same as no 8; == is default operator
  10.   language "Haskell"     # same as no 9; parens are optional
  10.   language month == 1, year == 2010

 in (pseudo) SQL: 
    
   1. SELECT language, extension, sum(editTime) FROM stats GROUP BY language, extension
   2. SELECT language, extension, sum(editTime) FROM stats GROUP BY language, extension
   3. SELECT language, extension, sum(editTime) FROM stats GROUP BY language
   4. SELECT language, extension, editTime FROM stats
   5. SELECT language, sum(editTime) FROM stats GROUP BY language LIMIT 5
   6. SELECT language, sum(editTime) AS e FROM stats GROUP BY language ORDER BY e ASC
   7. SELECT language, sum(editTime) FROM stats WHERE language = "Haskell" GROUP BY language
   8. SELECT language, sum(editTime) FROM stats WHERE language = "Haskell" GROUP BY language
   9. SELECT language, sum(editTime) FROM stats WHERE language = "Haskell" GROUP BY language
  10. SELECT language, sum(editTime) FROM stats WHERE month == 1 AND year == 2010 GROUP BY language 
 
 Grammar:

   COMMAND     := QUERY | ASSIGNMENT
   QUERY       := ε | SUBQUERIES
   SUBQUERIES  := SUBQUERY | SUBQUERIES * SUBQUERY
   SUBQUERY    := GROUPING INDEX CONSTRAINTS ORDER? LIMIT? | IDENT
   ASSIGNMENT  := IDENT := QUERY
   LIMIT       := limit DIGIT+
   ORDER       := ascending | descending | asc | desc
   GROUPING    := group | & | nogroup | !
   INDEX       := extension | language | project | filename | year | month | day | dow | doy 
   CONSTRAINTS := ( CONS ) | CONS
   CONS        := ε | CONSTRAINT | CONS , CONSTRAINT 
   CONSTRAINT  := INDEX OPERATOR EXPR | OPERATOR EXPR | EXPR
   OPERATOR    := == | = | <= | < | > | >= | != | /=
   EXPR        := DIGIT+ | STRING
   IDENT       := [A-Z]+[a-zA-Z0-9]*


   tokens  := extension, language, project, filename, year, month, day, dow, doy
            , ascending, descending, asc, desc, limit, '(', ')', ',', '==', '<='
            , '<', '>', '>=', '!=', '/=', '*', '&', '!'
-}


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
import Control.Monad


-- Each query is represented by sub queries, 
-- each of which adds a new level to the 
-- generated tree.
--
type Commands   = (Queries, Env)
type ExecResult = ([StatsTree], Env)
type Queries    = [Query]
type Env        = D.Map String Query
type Query      = [SubQuery]
type SubQuery   = (Header, View, Constraint, Group)
type Constraint = Stats -> (Stats, Stats)
type View       = EditStats -> String
type Group      = Stats -> [Stats]


commandsFromFiles :: Env -> [FilePath] -> IO (E Commands)
commandsFromFiles env []     = return $ Ok ([], env)
commandsFromFiles env (f:fp) = do o <- commandsFromFile env f
                                  case o of 
                                    Ok (q,e) -> do o2 <- commandsFromFiles e fp 
                                                   case o2 of 
                                                     Ok (q2,e) -> return (Ok (q ++ q2, e))
                                                     Failed s -> return (Failed s)
                                    Failed s -> return (Failed s)

commandsFromFile :: Env -> FilePath -> IO (E Commands)
commandsFromFile env = fmap (>>= fromQCommands env) . parseFile

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



-- Executing a Query 
-- A Query works on Stats, which is a flat list of EditStats.
-- For our results to be meaningful we need to 
-- create a StatsTree, where each level represents another
-- subquery. Executing a query can thus be seen 
-- as building a tree from a list. 
--
executeCommands :: Stats -> Commands -> ExecResult
makeTree        :: Query -> Stats -> StatsTree
makeTree'       :: Stats -> Query -> Time -> [StatsTree]
makeNode        :: String -> Stats -> Query -> StatsTree
treeFromQuery   :: String -> Env -> Stats -> E ExecResult

executeCommands st (q, env) = (map (flip makeTree st) q, env)

makeTree [] s = Root [] [] "" (0,0,0)
makeTree q  s = Root nodes headers title time
  where nodes = makeTree' s q (0,0,0)
        headers = map (\(h,_,_,_) -> h) q ++ ["Edit Time"]
        title = concat . intersperse " / " $ reverse headers
        time = sumTime' (map f nodes)
        f (Node _ t _ _) = t
        f _ = (0,0,0)

makeTree' s []            t = [Leaf t]
makeTree' s ((_,v,c, g):cs) t = pruneNodeTrees nodes
  where (yes, no) = c s
        nodes = map (\gr -> makeNode (v . head $ gr) gr cs) (g yes)


-- Pruning is pretty easy because we need to remove 
-- the trees that don't have any leafs in them, 
-- and we already keep that information in the Nodes.
--
pruneNodeTrees :: [StatsTree] -> [StatsTree]
pruneNodeTrees = filter ((/=0) . f) 
  where f (Node i _ _ _) = i
        f _              = 1

-- A Node is made of an Int describing the number of leafs,
-- a string that is used for printing, and of course its children.
--
makeNode s yes cs = Node (n tr) time s tr
  where tr = makeTree' yes cs time
        time = sumTime yes
        n [] = 0       -- Count children
        n ((Node i _ _ _):cs) = i + n cs
        n ((Leaf _):cs) = 1 + n cs
       

-- A helper function that builds a StatsTree out 
-- of a String in the Query language.
--
treeFromQuery s env st = executeCommands st <$> parseCommands s `thenE` fromQCommands env



-- Interactive Query prompt using editline
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
           Just s       -> case lookup s replCommands of 
                             Just (c, _) -> c stats env po
                             Nothing -> eval stats env po s

eval stats env po s = do 
  addHistory s
  case treeFromQuery s env stats of
    Ok (st, e) -> (putStrLn $ printTree po st) >> repl stats e po
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
fromQSubQuery    :: Env -> QSubQuery -> E Query
fromQIndex       :: QIndex -> (EditStats -> String)
addGrouping      :: Ord a => Bool -> (EditStats -> a) -> Group

fromQCommands env qs = fromQCommands' env [] qs
  where fromQCommands' env res []     = Ok (res, env)
        fromQCommands' env res (q:qs) = fromQCommand env q `thenE` f
          where f (Left qr) = fromQCommands' env (qr : res) qs 
                f (Right e) = fromQCommands' newEnv res qs
                   where newEnv = D.union e env
 

fromQCommand env (Left q)  = Left <$> fromQQuery env q
fromQCommand env (Right a) = Right <$> i a
  where i (QAssign s q) = flip (D.insert s) env <$> fromQQuery env q


-- First convert all the subqueries, then apply
-- ordering and limiting to the last grouping function
-- 
fromQQuery env []     = Ok []
fromQQuery env (c:cs) = fromQSubQuery env c `thenE` (\a -> (a++) <$> fromQQuery env cs)




-- Sub queries
--
fromQSubQuery _ q@(QSubQuery _ Ext   _ _ _ _) = makeQuery q (takeExtension . fileName)
fromQSubQuery _ q@(QSubQuery _ Lang  _ _ _ _) = makeQuery q language
fromQSubQuery _ q@(QSubQuery _ Proj  _ _ _ _) = makeQuery q project
fromQSubQuery _ q@(QSubQuery _ File  _ _ _ _) = makeQuery q fileName
fromQSubQuery _ q@(QSubQuery _ Year  _ _ _ _) = makeQuery q (year . edit)
fromQSubQuery _ q@(QSubQuery _ Month _ _ _ _) = makeQuery q (month . edit)
fromQSubQuery _ q@(QSubQuery _ Day   _ _ _ _) = makeQuery q (day . edit)
fromQSubQuery _ q@(QSubQuery _ Dow   _ _ _ _) = makeQuery q (dow . edit)
fromQSubQuery _ q@(QSubQuery _ Doy   _ _ _ _) = makeQuery q (doy . edit)
fromQSubQuery env (QCall s)                 = case D.lookup s env of 
                                                Just q -> Ok q
                                                Nothing -> Failed $ "Unknown definition `" ++ s ++ "'"

makeQuery (QSubQuery gr t c h o l) f  = Ok [(fromQAs h t, view, constraints, grouping)]
   where view = fromQIndex t
         constraints = fromQConstraints t c
         grouping = fromQLimit l . fromQOrder o . addGrouping gr f


fromQAs (As s) _     = s
fromQAs _      Ext   = "Extension"
fromQAs _      Lang  = "Language"
fromQAs _      Proj  = "Project"
fromQAs _      File  = "File Name"
fromQAs _      Year  = "Year"
fromQAs _      Month = "Month"
fromQAs _      Day   = "Day of the Month"
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
fromQIndex File  = fileName
fromQIndex Year  = show . year . edit
fromQIndex Month = getMonth . month . edit
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

makePred i (QC File  op e) = fromQOper op (fromQExpr e) . fileName
makePred i (QC Month op e) = numStringC op e month getMonth
makePred i (QC Dow   op e) = numStringC op e dow getDow
makePred i (QC Year  op e) = numericalC op year e
makePred i (QC Day   op e) = numericalC op day e
makePred i (QC Doy   op e) = numericalC op doy e
makePred i (QC ind   op e) = fromQOper op (fromQExpr e) . fromQIndex ind
makePred i (QCOE     op e) = makePred i (QC i op e)
makePred i (QCE         e) = makePred i (QC i QE e)


-- helper functions; to do the conversion
stringC op s f    = fromQOper op (map toUpper s) . map toUpper . f . edit
numericalC op g e = fromQOper op (read (fromQExpr e)) . g . edit
maybeC op g h e   = maybe False (fromQOper op (fromQExpr e) . h) . g

numStringC op (QInt i) num _      = fromQOper op i . num . edit
numStringC op (QString s) num str = stringC op s (str . num)


-- Expressions 
--
fromQExpr :: QExpr -> String
fromQExpr (QInt i) = show i
fromQExpr (QString s) = s


-- Operators are written flipped around to make it easier 
-- to write in a point-free style (see above)
--
fromQOper :: Ord a => QOper -> (a -> a -> Bool)
fromQOper QL  = (>)  
fromQOper QG  = (<)  
fromQOper QLE = (>=) 
fromQOper QGE = (<=) 
fromQOper QE  = (==)
fromQOper QNE = (/=)


