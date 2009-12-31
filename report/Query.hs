module Query (


             ) where

import Stats
import Printers

import Maybe


type Queries = [Query]
type Query = (View, Constraint)
type Constraint = Stats -> (Stats, Stats)
type View = EditStats -> String




makeConstraint :: Pred EditStats -> Constraint
makeConstraint p = con ([], [])
  where con (yes, no) [] = (yes, no)
        con (yes, no) (s:st) = con n st
          where n = if p s then (s : yes, no) else (yes, s:no)

langQ = (showLanguage "NONE" . language, makeConstraint (isJust . language))


makeTree :: Stats -> Queries -> StatsTree
makeTree s q = Root (makeTree' s q)
                  

makeTree' :: Stats -> Queries -> [StatsTree]
makeTree' s []         = []
makeTree' s ((v,c):cs) = if null yes then [makeNode (const "None") undefined no cs]
                                     else map (\e -> makeNode v e yes cs) yes
   where (yes, no) = c s

makeNode :: View -> EditStats -> Stats -> Queries -> StatsTree
makeNode v s yes cs = if null cs then Node 1 (v s) [Leaf (sumTime yes)]
                                 else Node 0 (v s) (makeTree' yes cs)


{-
makeNode :: EditStats -> Query -> Stats -> Queries -> StatsTree
makeNode e (v, c) (s, n) [] = Node 1 (v e) [Leaf (sumTime n)]
makeNode e (v, c) (s, n) cs = Node 0 (v e) [(makeTree s cs)]

-}
