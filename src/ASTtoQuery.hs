module ASTtoQuery where 

--import QueryParser
import Control.Applicative ((<$>))
import Data.Char (toUpper)
import Data.List
import Data.Function
import QueryAST
import Maybe (isJust)
import Printers
import Stats
import System.FilePath
import Text.Regex
import qualified Data.Map as D

-- * Queries after AST processing.
-- 
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


-- Convert AST to Query
-- We have to transform the parsing result (QCommands) into 
-- SubQueries (ie. a view function, a constraint and a 
-- group function) and an Environment.
--

fromQCommands    :: Env -> QCommands -> E Commands
fromQCommands env qs = fromQCommands' env [] qs
  where fromQCommands' env res []     = Ok (res, env)
        fromQCommands' env res (q:qs) = fromQCommand env q >>= f
          where f (Left qr) = fromQCommands' env (qr : res) qs 
                f (Right e) = fromQCommands' newEnv res qs
                   where newEnv = D.union e env
 

fromQCommand     :: Env -> QCommand -> E (Either Query Env)
fromQCommand env (Left q)  = Left <$> fromQQuery env q
fromQCommand env (Right a) = Right <$> i a
  where i (QAssign s q) = flip (D.insert s) env <$> fromQQuery env q


fromQQuery       :: Env -> QQuery -> E Query
fromQQuery env ([], s)    = Ok ([], s)
fromQQuery env ((c:cs),s) = fromQSubQuery env c >>= 
                             (\a -> flip (,) s . (a++) . fst <$> fromQQuery env (cs,s))




-- Sub queries
--
fromQSubQuery    :: Env -> QSubQuery -> E [SubQuery]
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
fromQIndex       :: QIndex -> (EditStats -> String)
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
addGrouping :: Ord a => Bool -> (EditStats -> a) -> Group
addGrouping False _ = map (:[]) -- dontGroup
addGrouping True  f = groupWith f . sortBy (compare `on` f)


addToGrouping [] _ = error "Empty query, can't add to grouping"
addToGrouping cs f = init cs ++ [add (last cs)]
  where add (v, cs, gr) = (v, cs, f . gr)



-- A Constraint separates Stats matching 
-- a predicate from the ones that don't
-- (P(s), ¬P(s))
--
makeConstraint :: Pred EditStats -> Constraint
makeConstraint p = con ([], [])
  where con (yes, no) [] = (yes, no)
        con (yes, no) (s:st) = con n st
          where n = if p s then (s : yes, no) else (yes, s:no)

-- QConstraints to Constraints
-- Individual QConstraints are first converted to predicates
-- and get turned into one Constraint using makeConstraint.
--


fromQConstraints :: QIndex -> [QConstraint] -> Constraint
fromQConstraints i qc = makeConstraint $ foldr f (const True) preds
  where f a b p       = a p && b p 
        preds         = map (makePred i) qc

makePred :: QIndex -> QConstraint -> Pred EditStats
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
fromQOper QREG = \m -> isJust . matchRegex (mkRegex m) 

