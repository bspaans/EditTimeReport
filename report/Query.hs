module Query ( makeTree, interactiveQueries ) where

{-
 
 Examples of the embedded Query Language:

   1.   language * extension
   2.   group language * extension
   3.   group language * group extension
   4.   group language limit 5
   5.   group language ascending
   6.   group language (month == 1, year == 2010)
   7.   group language (time >= 3 months ago)

 Last example is not yet implemented.

 in (pseudo) SQL: 
    
   1. SELECT language, extension, editTime FROM stats
   2. SELECT language, extension, sum(editTime) FROM stats GROUP BY language
   3. SELECT language, extension, sum(editTime) FROM stats GROUP BY language, extension
   4. SELECT language, sum(editTime) FROM stats GROUP BY language LIMIT 5
   5. SELECT language, sum(editTime) AS e FROM stats GROUP BY language ORDER BY e ASC
   6. SELECT language, sum(editTime) FROM stats WHERE month == 1 AND year == 2010 GROUP BY language 
   7. SELECT language, sum(editTime) FROM stats WHERE month >= MONTH() - 3 and year == YEAR() GROUP BY language
 
 Grammar:

   QUERY       := ε | SUBQUERIES ORDER? LIMIT?
   SUBQUERIES  := SUBQUERY | SUBQUERIES * SUBQUERY
   SUBQUERY    := group? INDEX CONSTRAINTS
   LIMIT       := limit DIGIT+
   ORDER       := ascending | descending | asc | desc
   INDEX       := extension | language | project | filename | year | month | day | dow | doy 
   CONSTRAINTS := ε | ( CONS )
   CONS        := CONSTRAINT | CONS , CONSTRAINT
   CONSTRAINT  := INDEX OPERATOR VALUE
   OPERATOR    := == | = | <= | < | > | >= | != | /=
   VALUE       := DIGIT+ | STRING


   tokens  := extension, language, project, filename, year, month, day, dow, doy
            , ascending, descending, asc, desc, limit, '(', ')', ',', '==', '<='
            , '<', '>', '>=', '!=', '/=', '*'
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
-- a string that is used for printing and of course its children.
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
interactiveQueries stats = 
  do maybeLine <- readline "> " 
     case maybeLine of
       Nothing     -> do putStr "\n" ; return ()
       Just "exit" -> do putStr "\n" ; return ()
       Just s -> do addHistory s
                    case treeFromQuery s stats of
                       Ok a     -> putStrLn $ treeToString a
                       Failed e -> putStrLn e
                    interactiveQueries stats



-- Convert Parsed QQuery to Query
--
fromQQuery       :: QQuery -> Query
fromQSubQuery    :: QSubQuery -> SubQuery
fromQConstraints :: [QConstraint] -> Constraint
fromQConstraint  :: QConstraint -> Pred EditStats
fromQIndex       :: QIndex -> (EditStats -> String)
fromQExpression  :: QExpr -> String
addGrouping      :: Ord a => Bool -> (EditStats -> a) -> Group
fromQOper        :: Ord a => QOper -> (a -> a -> Bool)


fromQQuery (qs, order, limit) = fromQLimit (fromQOrder (map fromQSubQuery qs) order) limit


fromQSubQuery q@(QSubQuery _ Ext   _ )  = makeQuery q extInformation
fromQSubQuery q@(QSubQuery _ Lang  _ )  = makeQuery q language
fromQSubQuery q@(QSubQuery _ Proj  _ )  = makeQuery q project
fromQSubQuery q@(QSubQuery _ File  _ )  = makeQuery q fileName
fromQSubQuery q@(QSubQuery _ Year  _ )  = makeQuery q (year . edit)
fromQSubQuery q@(QSubQuery _ Month _ )  = makeQuery q (month . edit)
fromQSubQuery q@(QSubQuery _ Day   _ )  = makeQuery q (day . edit)
fromQSubQuery q@(QSubQuery _ Dow   _ )  = makeQuery q (dow . edit)
fromQSubQuery q@(QSubQuery _ Doy   _ )  = makeQuery q (doy . edit)


makeQuery (QSubQuery gr t c) f  = (fromQIndex t, fromQConstraints c, addGrouping gr f)

fromQOrder [] _         = []
fromQOrder cs NoOrder   = cs
fromQOrder cs Asc       = addToGrouping cs (sortBy (compare `on` sumTime))
fromQOrder cs Desc      = addToGrouping cs (reverse . sortBy (compare `on` sumTime))


fromQLimit [] _         = []
fromQLimit cs NoLimit   = cs
fromQLimit cs (Limit i) = addToGrouping cs (take i)


addToGrouping [] _ = error "Empty query, can't add to grouping"
addToGrouping cs f = init cs ++ [add (last cs)]
  where add (v, cs, gr) = (v, cs, f . gr)


addGrouping False _ = dontGroup
addGrouping True  f = groupWith f . sortBy (compare `on` f)



fromQConstraints qc = makeConstraint $ foldr (\a b p -> a p && b p) (const True) q
  where q           = map fromQConstraint qc


-- Convert the parsed constraints into a predicate
--
fromQConstraint (QConstraint Ext op e) = maybeConstraint op extInformation id e
fromQConstraint (QConstraint Lang op e) = maybeConstraint op language snd e
fromQConstraint (QConstraint Proj op e) = maybeConstraint op project snd e
fromQConstraint (QConstraint File op e) = f . fileName 
  where f d = fromQOper op d $ fromQExpression e
fromQConstraint (QConstraint Year op e) = numericalConstraint op year e
fromQConstraint (QConstraint Month op (QString s)) = f . map toUpper . getMonth . month . edit
  where f d = fromQOper op d $ map toUpper s
fromQConstraint (QConstraint Month op (QInt i)) = f . month . edit
  where f d = fromQOper op d i
fromQConstraint (QConstraint Day op e) = numericalConstraint op day e
fromQConstraint (QConstraint Dow op (QString s)) = f . map toUpper . getDow . dow . edit
  where f d = fromQOper op d $ map toUpper s
fromQConstraint (QConstraint Dow op (QInt i)) = f . dow . edit
  where f d = fromQOper op d i
fromQConstraint (QConstraint Doy op e) = numericalConstraint op doy e

numericalConstraint op g e = f . g . edit  
  where f d = fromQOper op d $ read (fromQExpression e)

maybeConstraint op g h e = maybe False f . g
  where f d = fromQOper op (h d) $ fromQExpression e


fromQIndex Ext   = fromMaybe "Unknown extension" . extInformation
fromQIndex Lang  = maybe "Unknown language" snd . language
fromQIndex Proj  = maybe "Unknown project" snd . project
fromQIndex File  = fileName
fromQIndex Year  = show . year . edit
fromQIndex Month = getMonth . month . edit
fromQIndex Day   = show . day . edit
fromQIndex Dow   = getDow . dow . edit
fromQIndex Doy   = show . doy . edit


fromQExpression (QInt i) = show i
fromQExpression (QString s) = s


fromQOper QL  = (<)
fromQOper QG  = (>)
fromQOper QLE = (<=)
fromQOper QGE = (>=)
fromQOper QE  = (==)
fromQOper QNE = (/=)


dontGroup = map (:[]) 


