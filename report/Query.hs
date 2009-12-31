module Query (
             makeTree


             ) where

import Stats
import Printers
import Data.List
import Data.Function

import Maybe

type Queries = [Query]
type Query = [QueryPart]
type QueryPart = (View, Constraint, Group)
type Constraint = Stats -> (Stats, Stats)
type View = EditStats -> String
type Group = Stats -> [Stats]



makeConstraint :: Pred EditStats -> Constraint
makeConstraint p = con ([], [])
  where con (yes, no) [] = (yes, no)
        con (yes, no) (s:st) = con n st
          where n = if p s then (s : yes, no) else (yes, s:no)

langQ = (showLanguage "NONE" . language, makeConstraint (isJust . language), groupWith language)


makeTree :: Stats -> Query -> StatsTree
makeTree s q = Root (makeTree' s q (0,0,0))
                  
-- TODO: add sorting and totals?
makeTree' :: Stats -> Query -> Time -> [StatsTree]
makeTree' s []         t  = [Leaf t]
makeTree' s ((v,c, g):cs) t = map (\gr -> makeNode (v . head $ gr) (sumTime gr) yes cs) (g . sortBy (compare `on` v) $ yes)
                                      ++ [makeNode "None" (sumTime no) no cs]
   where (yes, no) = c s

makeNode :: String -> Time -> Stats -> Query -> StatsTree
makeNode s t yes cs = Node 0 s (makeTree' yes cs t)

{-
 
 Examples

 Constraint Language:

   1.   language * extension
   2.   group language * extension
   3.   group language * group extension
   4.   group language limit 5
   5.   group language ascending

 in (pseudo?) SQL: 
    
   1. SELECT language, extension, editTime FROM stats
   2. SELECT language, extension, sum(editTime) FROM stats GROUP BY language
   3. SELECT language, extension, sum(editTime) FROM stats GROUP BY language, extension
   4. SELECT language, sum(editTime) FROM stats GROUP BY language LIMIT 5
   5. SELECT language, sum(editTime) AS e FROM stats GROUP BY language ORDER BY e ASC
 
 Grammar:

   The terminals are not case sensitive, so `GROUP' is the same as `group'

   QUERY   := ε | Q QPREFIX?
   Q       := T | Q * T
   T       := group? TABLE 
   QPREFIX := LIMIT | ORDER
   LIMIT   := limit DIGIT+
   ORDER   := ascending | descending | asc | desc
   TABLE   := extension | language | project | filename | year | month | day | dow | doy 



 -}
