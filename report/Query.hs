module Query ( makeTree, interactiveQueries ) where

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

   QUERY       := ε | SUBQUERIES
   SUBQUERIES  := SUBQUERY | SUBQUERIES * SUBQUERY
   SUBQUERY    := GROUPING INDEX CONSTRAINTS ORDER? LIMIT?
   LIMIT       := limit DIGIT+
   ORDER       := ascending | descending | asc | desc
   GROUPING    := group | & | nogroup | !
   INDEX       := extension | language | project | filename | year | month | day | dow | doy 
   CONSTRAINTS := ( CONS ) | CONS
   CONS        := ε | CONSTRAINT | CONS , CONSTRAINT 
   CONSTRAINT  := INDEX OPERATOR EXPR | OPERATOR EXPR | EXPR
   OPERATOR    := == | = | <= | < | > | >= | != | /=
   EXPR        := DIGIT+ | STRING


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
import System.Directory
import System.FilePath
import Control.Monad


-- Each query is represented by sub queries, 
-- each of which adds a new level to the 
-- generated tree.
--
type Queries    = [Query]
type Env        = D.Map String Query
type Query      = [SubQuery]
type SubQuery   = (View, Constraint, Group)
type Constraint = Stats -> (Stats, Stats)
type View       = EditStats -> String
type Group      = Stats -> [Stats]



-- A Constraint separates Stats matching 
-- a predicate from the ones that don't
-- (P(s), ¬P(s))
--
makeConstraint :: Pred EditStats -> Constraint
makeConstraint p = con ([], [])
  where con (yes, no) [] = (yes, no)
        con (yes, no) (s:st) = con n st
          where n = if p s then (s : yes, no) else (yes, s:no)


-- Executing Queries
--
-- fromQCommands :: QCommands -> Env -> Stats -> ([StatsTree], Env)
-- fromQCommand :: QCommand -> Env -> Stats -> Either [StatsTree] Env


-- Executing a Query 
-- A Query works on Stats, which is a flat list of EditStats.
-- For our results to be meaningful we need to 
-- create a StatsTree, where each level represents another
-- subquery. Executing a query can thus be seen 
-- as building a tree from a list. 
--
makeTree      :: Query -> Stats -> StatsTree
makeTree'     :: Stats -> Query -> Time -> [StatsTree]
makeNode      :: String -> Stats -> Query -> StatsTree
treeFromQuery :: String -> Env -> Stats -> E (Either StatsTree Env)

makeTree [] s = Root 0 []
makeTree q  s = Root (length q + 1) (makeTree' s q (0,0,0))

makeTree' s []            t = [Leaf t]
makeTree' s ((v,c, g):cs) t = pruneNodeTrees nodes
  where (yes, no) = c s
        nodes = map (\gr -> makeNode (v . head $ gr) gr cs) (g yes)


-- Pruning is pretty easy because we need to remove 
-- the trees that don't have any leafs in them, 
-- and we already keep that information in the Nodes.
--
pruneNodeTrees :: [StatsTree] -> [StatsTree]
pruneNodeTrees = filter ((/=0) . f) 
  where f (Node i _ _) = i
        f _            = 1

-- A Node is made of an Int describing the number of leafs,
-- a string that is used for printing, and of course its children.
--
makeNode s yes cs = Node (n tr) s tr
  where tr = makeTree' yes cs (sumTime yes)
        n [] = 0       -- Count children
        n ((Node i _ _):cs) = i + n cs
        n ((Leaf _):cs) = 1 + n cs
       

-- A helper function that builds a StatsTree out 
-- of a String in the Query language.
--
treeFromQuery s env st = parsed
  where parsed         = f <$> parseQuery s `thenE` fromQCommand env
        f (Left s)     = Left $ flip makeTree st s
        f (Right e)    = Right e



-- Interactive Query prompt using editline
--
interactiveQueries :: Stats -> IO()
interactiveQueries stats = do putStrLn (unlines ["Time Report 1.0a, interactive session"
                                               , "Copyright 2009-2010, Bart Spaans"
                                               , "Type \"help\" for more information"])
                              loc <- historyLocation
                              readHistory loc
                              interactiveQ' (D.fromList [])
  where interactiveQ' env = do
         setCompletionEntryFunction (Just $ qCompleter env)
         maybeLine <- readline "> " 
         case maybeLine of
           Nothing     -> do putStr "\n" ; onExit ; return ()
           Just ""     -> interactiveQ' env
           Just "exit" -> do putStr "\n" ; onExit ; return ()
           Just s -> do addHistory s
                        case treeFromQuery s env stats of
                           Ok a     -> case a of 
                                         Left t  -> putStrLn $ treeToString t
                                         Right e -> putStrLn "Definition added" >> interactiveQ' e
                           Failed e -> putStrLn e
                        interactiveQ' env

onExit = do loc <- historyLocation
            writeHistory loc

historyLocation :: IO FilePath
historyLocation = (</> ".report_history") <$> getUserDocumentsDirectory

qCompleter :: Env -> String -> IO [String]
qCompleter env s = return (filter (startsWith s) known)
  where known = ["extension", "language", "project", "filename"
               , "year", "month", "day", "dow", "doy", "limit"
               , "asc", "desc"] ++ D.keys env


-- Convert AST to Query
-- We have to transform the parsing result (QQuery) into 
-- SubQueries (ie. a view function, a constraint and a 
-- group function)
--
fromQCommand     :: Env -> QCommand -> E (Either Query Env)
fromQQuery       :: Env -> QQuery -> E Query
fromQSubQuery    :: Env -> QSubQuery -> E Query
fromQIndex       :: QIndex -> (EditStats -> String)
addGrouping      :: Ord a => Bool -> (EditStats -> a) -> Group


fromQCommand env (Left q)  = Left <$> fromQQuery env q
fromQCommand env (Right a) = Right <$> i a
  where i (QAssign s q) = flip (D.insert s) env <$> (fromQQuery env q) 

-- First convert all the subqueries, then apply
-- ordering and limiting to the last grouping function
-- 
fromQQuery env []     = Ok []
fromQQuery env (c:cs) = fromQSubQuery env c `thenE` (\a -> (a++) <$> (fromQQuery env cs))




-- Sub queries
--
fromQSubQuery _ q@(QSubQuery _ Ext   _ _ _) = makeQuery q extInformation
fromQSubQuery _ q@(QSubQuery _ Lang  _ _ _) = makeQuery q language
fromQSubQuery _ q@(QSubQuery _ Proj  _ _ _) = makeQuery q project
fromQSubQuery _ q@(QSubQuery _ File  _ _ _) = makeQuery q fileName
fromQSubQuery _ q@(QSubQuery _ Year  _ _ _) = makeQuery q (year . edit)
fromQSubQuery _ q@(QSubQuery _ Month _ _ _) = makeQuery q (month . edit)
fromQSubQuery _ q@(QSubQuery _ Day   _ _ _) = makeQuery q (day . edit)
fromQSubQuery _ q@(QSubQuery _ Dow   _ _ _) = makeQuery q (dow . edit)
fromQSubQuery _ q@(QSubQuery _ Doy   _ _ _) = makeQuery q (doy . edit)
fromQSubQuery env (QCall s)                 = case D.lookup s env of 
                                                Just q -> Ok q
                                                Nothing -> Failed $ "Unknown definition `" ++ s ++ "'"

makeQuery (QSubQuery gr t c o l) f  = Ok [(view, constraints, grouping)]
   where view = fromQIndex t
         constraints = fromQConstraints t c
         grouping = fromQLimit l . fromQOrder o . addGrouping gr f


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
fromQIndex Ext   = fromMaybe "Unknown extension" . extInformation
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


