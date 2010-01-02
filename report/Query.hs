module Query ( makeTree) where

import Stats
import Printers
import QueryParser
import Data.List
import Data.Function

import Maybe

type Queries = [Query]
type Query = [SubQuery]
type SubQuery = (View, Constraint, Group)
type Constraint = Stats -> (Stats, Stats)
type View = EditStats -> String
type Group = Stats -> [Stats]



fromQQuery :: QQuery -> Query
fromQQuery (qs, postfix) = map fromQSubQuery qs

fromQSubQuery :: QSubQuery -> SubQuery
fromQSubQuery (QSubQuery gr Ext cons) = (fromQTable Ext, fromQConstraints cons
                                        , addGrouping gr extInformation)

addGrouping :: Eq a => Bool -> (EditStats -> a) -> Group
addGrouping False _ = dontGroup
addGrouping True  f = groupWith f
                                          
fromQConstraints :: [QConstraint] -> Constraint
fromQConstraints qc = makeConstraint $ foldr (\a b -> \p -> a p && b p) (const True) q
  where q = map fromQConstraint qc

fromQConstraint :: QConstraint -> Pred EditStats
fromQConstraint (QConstraint Ext oper expr) = maybe False (\e -> fromQOper oper e $ fromQExpression expr) . extInformation 

fromQTable :: QTable -> (EditStats -> String)
fromQTable Ext  = fromMaybe "" . extInformation
fromQTable Lang = maybe "" fst . language

fromQExpression :: QExpr -> String
fromQExpression (QInt i) = show i

fromQOper QL = (<)
fromQOper QLE = (<=)



dontGroup xs = [xs]

makeConstraint :: Pred EditStats -> Constraint
makeConstraint p = con ([], [])
  where con (yes, no) [] = (yes, no)
        con (yes, no) (s:st) = con n st
          where n = if p s then (s : yes, no) else (yes, s:no)

langQ = (showLanguage "NONE" . language, makeConstraint (isJust . language), groupWith language)


makeTree :: Query -> Stats -> StatsTree
makeTree q s = Root (makeTree' s q (0,0,0))
                  
-- TODO: add sorting and totals?
makeTree' :: Stats -> Query -> Time -> [StatsTree]
makeTree' s []         t  = [Leaf t]
makeTree' s ((v,c, g):cs) t = map (\gr -> makeNode (v . head $ gr) (sumTime gr) yes cs) (g . sortBy (compare `on` v) $ yes)
                                      ++ [makeNode "None" (sumTime no) no cs]
   where (yes, no) = c s

makeNode :: String -> Time -> Stats -> Query -> StatsTree
makeNode s t yes cs = Node 0 s (makeTree' yes cs t)


treeFromQuery :: String -> Stats -> StatsTree
treeFromQuery = makeTree . fromQQuery . parseQuery 

{-
 
 Examples

 Constraint Language:

   1.   language * extension
   2.   group language * extension
   3.   group language * group extension
   4.   group language limit 5
   5.   group language ascending
   6.   group language (time >= 3 months ago)

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
