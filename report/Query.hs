module Query (
             makeTree


             ) where

import Stats
import Printers
import Data.List

import Maybe


type Queries = [Query]
type Query = (View, Constraint, Group)
type Constraint = Stats -> (Stats, Stats)
type View = EditStats -> String
type Group = Stats -> [Stats]




makeConstraint :: Pred EditStats -> Constraint
makeConstraint p = con ([], [])
  where con (yes, no) [] = (yes, no)
        con (yes, no) (s:st) = con n st
          where n = if p s then (s : yes, no) else (yes, s:no)

langQ = (showLanguage "NONE" . language, makeConstraint (isJust . language), groupWith language)


makeTree :: Stats -> Queries -> StatsTree
makeTree s q = Root (makeTree' s q (0,0,0))
                  

makeTree' :: Stats -> Queries -> Time -> [StatsTree]
makeTree' s []         t  = [Leaf t]
makeTree' s ((v,c, g):cs) t = if null yes then [makeNode (const "None") undefined (sumTime no) no cs]
                                        else map (\e -> makeNode v e (editTime e) (filter (/=e) yes) cs) yes 
                                            ++ [makeNode (const "None") undefined (sumTime no) no cs]
   where (yes, no) = c s

makeNode :: View -> EditStats -> Time -> Stats -> Queries -> StatsTree
makeNode v s t yes cs = Node 0 (v s) (makeTree' yes cs t)

