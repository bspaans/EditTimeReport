module Query ( makeTree, interactiveQueries ) where

import Stats
import Printers
import QueryParser
import Data.List
import Data.Function
import Maybe
import Control.Applicative
import System.Console.Editline.Readline

type Queries    = [Query]
type Query      = [SubQuery]
type SubQuery   = (View, Constraint, Group)
type Constraint = Stats -> (Stats, Stats)
type View       = EditStats -> String
type Group      = Stats -> [Stats]



-- Query prompt
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
                       Failed e -> putStrLn $ "Parse error: " ++ e
                    interactiveQueries stats



-- Convert Parsed QQuery to Query
--
fromQQuery       :: QQuery -> Query
fromQSubQuery    :: QSubQuery -> SubQuery
fromQConstraints :: [QConstraint] -> Constraint
fromQConstraint  :: QConstraint -> Pred EditStats
fromQTable       :: QTable -> (EditStats -> String)
fromQExpression  :: QExpr -> String
addGrouping      :: Ord a => Bool -> (EditStats -> a) -> Group
fromQOper        :: Ord a =>  QOper -> (a -> a -> Bool)


fromQQuery (qs, postfix) = map fromQSubQuery qs


fromQSubQuery (QSubQuery gr Ext   cons )  = makeQuery gr Ext   cons extInformation
fromQSubQuery (QSubQuery gr Lang  cons )  = makeQuery gr Lang  cons language
fromQSubQuery (QSubQuery gr Proj  cons )  = makeQuery gr Proj  cons project
fromQSubQuery (QSubQuery gr File  cons )  = makeQuery gr File  cons fileName
fromQSubQuery (QSubQuery gr Year  cons )  = makeQuery gr Year  cons (year . edit)
fromQSubQuery (QSubQuery gr Month cons )  = makeQuery gr Month cons (month . edit)
fromQSubQuery (QSubQuery gr Day   cons )  = makeQuery gr Day   cons (day . edit)
fromQSubQuery (QSubQuery gr Dow   cons )  = makeQuery gr Dow   cons (dow . edit)
fromQSubQuery (QSubQuery gr Doy   cons )  = makeQuery gr Doy   cons (doy . edit)


makeQuery gr t c f  = (fromQTable t, fromQConstraints c, addGrouping gr f)


addGrouping False _ = dontGroup
addGrouping True  f = groupWith f . sortBy (compare `on` f)


fromQConstraints qc = makeConstraint $ foldr (\a b p -> a p && b p) (const True) q
  where q           = map fromQConstraint qc


fromQConstraint (QConstraint Ext oper expr) = maybe False f . extInformation 
  where f e = fromQOper oper e $ fromQExpression expr


fromQTable Ext   = fromMaybe "None" . extInformation
fromQTable Lang  = maybe "None" snd . language
fromQTable Proj  = maybe "None" snd . project
fromQTable File  = fileName
fromQTable Year  = show . year . edit
fromQTable Month = getMonth . month . edit
fromQTable Day   = show . day . edit
fromQTable Dow   = getDow . dow . edit
fromQTable Doy   = show . doy . edit


fromQExpression (QInt i) = show i


fromQOper QL  = (<)
fromQOper QG  = (>)
fromQOper QLE = (<=)
fromQOper QGE = (>=)
fromQOper QE  = (==)
fromQOper QNE = (/=)


dontGroup = map (:[]) 



-- Make Constraint out of predicate
--
makeConstraint :: Pred EditStats -> Constraint
makeConstraint p = con ([], [])
  where con (yes, no) [] = (yes, no)
        con (yes, no) (s:st) = con n st
          where n = if p s then (s : yes, no) else (yes, s:no)



-- Get the Tree corresponding to the Query on Stats
--
makeTree      :: Query -> Stats -> StatsTree
makeTree'     :: Stats -> Query -> Time -> [StatsTree]
makeNode      :: String -> Stats -> Query -> StatsTree
treeFromQuery :: String -> Stats -> E StatsTree

makeTree q s  = Root (makeTree' s q (0,0,0))
 

-- TODO: add sorting and totals
--
makeTree' s []            t = [Leaf t]
makeTree' s ((v,c, g):cs) t = 
  if null yes then nomatch
    else map (\gr -> makeNode (v . head $ gr) gr cs) (g yes) ++ nomatch
  where (yes, no) = c s
        nomatch   = if null no then [] else [makeNode "None" no cs]


makeNode s yes cs = Node (n tr) s tr
  where tr = makeTree' yes cs (sumTime yes)
        n [] = 0       -- Count children
        n ((Node i _ _):cs) = i + n cs
        n ((Leaf _):cs) = 1 + n cs
 

treeFromQuery s st = flip makeTree st . fromQQuery <$>  parseQuery s


{-
 
 Examples

 Constraint Language:

   1.   language * extension
   2.   group language * extension
   3.   group language * group extension
   4.   group language limit 5
   5.   group language ascending
   6.   group language (time >= 3 months ago)

 Last example is not yet implemented.

 in (pseudo?) SQL: 
    
   1. SELECT language, extension, editTime FROM stats
   2. SELECT language, extension, sum(editTime) FROM stats GROUP BY language
   3. SELECT language, extension, sum(editTime) FROM stats GROUP BY language, extension
   4. SELECT language, sum(editTime) FROM stats GROUP BY language LIMIT 5
   5. SELECT language, sum(editTime) AS e FROM stats GROUP BY language ORDER BY e ASC
   6. SELECT language, sum(editTime) FROM stats WHERE month >= MONTH() - 3 and year == YEAR() GROUP BY language
 
 Grammar:

   QUERY       := ε | SUBQUERIES QPREFIX?
   SUBQUERIES  := SUBQUERY | SUBQUERIES * SUBQUERY
   SUBQUERY    := group? TABLE CONSTRAINTS
   QPREFIX     := LIMIT | ORDER
   LIMIT       := limit DIGIT+
   ORDER       := ascending | descending | asc | desc
   TABLE       := extension | language | project | filename | year | month | day | dow | doy 
   CONSTRAINTS := ε | ( CONS )
   CONS        := CONSTRAINT | CONS , CONSTRAINT
   CONSTRAINT  := TABLE OPERATOR VALUE
   OPERATOR    := == | <= | < | > | >= | != | /=
   VALUE       := NATURAL | STRING 


   tokens  := extension, language, project, filename, year, month, day, dow, doy
            , ascending, descending, asc, desc, limit, '(', ')', ',', '==', '<='
            , '<', '>', '>=', '!=', '/=', '*'
 -}

