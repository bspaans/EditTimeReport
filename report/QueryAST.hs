module QueryAST ( QQuery, QConstraint(..), QSubQuery(..)
                , QOper(..), QExpr(..), QOrder(..)
                , QLimit(..), QIndex(..), E(..)
                , returnE, thenE, failE, catchE
                ) where


-- A straight forward AST for the Query DSL
--
type QQuery      = ([QSubQuery], QOrder, QLimit)
data QSubQuery   = QSubQuery Bool QIndex [QConstraint]
data QIndex      = Ext | Lang | Proj | File | Year
                 | Month | Day | Dow | Doy deriving (Eq, Show)
data QConstraint = QC QIndex QOper QExpr | QCOE QOper QExpr | QCE QExpr
data QOper       = QL | QLE | QG | QGE | QE | QNE
data QExpr       = QInt Int | QString String
data QOrder      = Asc | Desc | NoOrder
data QLimit      = Limit Int | NoLimit


-- A Succes/Failure data type used for parsing
--
data E a = Ok a | Failed String

instance Functor E where
  fmap f (Ok a)  = Ok $ f a
  fmap f (Failed e) = Failed e

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = 
   case m of 
     Ok a -> k a
     Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = 
   case m of
     Ok a -> Ok a
     Failed e -> k e

