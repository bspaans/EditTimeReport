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

   QUERY       := ε | SUBQUERIES ORDER? LIMIT?
   SUBQUERIES  := SUBQUERY | SUBQUERIES * SUBQUERY
   SUBQUERY    := GROUPING INDEX CONSTRAINTS
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
import Maybe
import Control.Applicative
import System.Console.Editline.Readline


-- Each query is represented by sub queries, 
-- each of which adds a new level to the 
-- generated tree.
--
type Queries    = [Query]
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
treeFromQuery :: String -> Stats -> E StatsTree

makeTree q s  = Root (makeTree' s q (0,0,0))

makeTree' s []            t = [Leaf t]
makeTree' s ((v,c, g):cs) t = 
  -- if nomatch gets (optionally) removed here, a pruneTree 
  -- function is needed in makeTree to remove dead paths
  if null yes then nomatch
    else map (\gr -> makeNode (v . head $ gr) gr cs) (g yes) ++ nomatch 
  where (yes, no) = c s
        nomatch   = if null no then [] else [makeNode "No match" no cs]


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
treeFromQuery s st = flip makeTree st . fromQQuery <$>  parseQuery s



-- Interactive Query prompt using editline
--
interactiveQueries :: Stats -> IO()
interactiveQueries stats = putStrLn (unlines ["Time Report 1.0a, interactive session"
                                             , "Copyright 2009-2010, Bart Spaans"
                                             , "Type \"help\" for more information"]) >> interactiveQ'
  where interactiveQ' = do
         maybeLine <- readline "> " 
         case maybeLine of
           Nothing     -> do putStr "\n" ; return ()
           Just ""     -> interactiveQ'
           Just "exit" -> do putStr "\n" ; return ()
           Just s -> do addHistory s
                        case treeFromQuery s stats of
                           Ok a     -> putStrLn $ treeToString a
                           Failed e -> putStrLn e
                        interactiveQ' 



-- Convert AST to Query
-- We have to transform the parsing result (QQuery) into 
-- SubQueries (ie. a view function, a constraint and a 
-- group function)
--
fromQQuery       :: QQuery -> Query
fromQSubQuery    :: QSubQuery -> SubQuery
fromQIndex       :: QIndex -> (EditStats -> String)
addGrouping      :: Ord a => Bool -> (EditStats -> a) -> Group


-- First convert all the subqueries, then apply
-- ordering and limiting to the last grouping function
-- 
fromQQuery (qs, order, limit) = limiting
  where subs = map fromQSubQuery qs
        ordering = fromQOrder subs order
        limiting = fromQLimit ordering limit


-- Ordering is done before passing the grouped Stats
-- on to the constraint function
--
fromQOrder [] _         = []
fromQOrder cs NoOrder   = cs
fromQOrder cs Asc       = addToGrouping cs (sortBy (compare `on` sumTime))
fromQOrder cs Desc      = addToGrouping cs (sortBy (flip compare `on` sumTime))


-- Limiting is done before passing the constraint 
-- function as well
--
fromQLimit [] _         = []
fromQLimit cs NoLimit   = cs
fromQLimit cs (Limit i) = addToGrouping cs (take i)


-- Sub queries
--
fromQSubQuery q@(QSubQuery _ Ext   _ )  = makeQuery q extInformation
fromQSubQuery q@(QSubQuery _ Lang  _ )  = makeQuery q language
fromQSubQuery q@(QSubQuery _ Proj  _ )  = makeQuery q project
fromQSubQuery q@(QSubQuery _ File  _ )  = makeQuery q fileName
fromQSubQuery q@(QSubQuery _ Year  _ )  = makeQuery q (year . edit)
fromQSubQuery q@(QSubQuery _ Month _ )  = makeQuery q (month . edit)
fromQSubQuery q@(QSubQuery _ Day   _ )  = makeQuery q (day . edit)
fromQSubQuery q@(QSubQuery _ Dow   _ )  = makeQuery q (dow . edit)
fromQSubQuery q@(QSubQuery _ Doy   _ )  = makeQuery q (doy . edit)

makeQuery (QSubQuery gr t c) f  = (fromQIndex t, fromQConstraints t c, addGrouping gr f)



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
  where f a b p     = a p && b p 
        preds       = map (makePred i) qc

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


