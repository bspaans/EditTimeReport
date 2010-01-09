module QueryAST where

import Control.Applicative

-- A straight forward AST for the Query DSL
--
type QCommands   = [QCommand]
type QCommand    = Either QQuery QAssign 
type QQuery      = [QSubQuery]
data QAssign     = QAssign String QQuery
data QSubQuery   = QSubQuery Bool QIndex [QConstraint] QAs QOrder QLimit | QCall String
data QIndex      = Ext | Lang | Proj | File | Year
                 | Month | Day | Dow | Doy deriving (Eq, Show)
data QConstraint = QC QIndex QOper QExpr | QCOE QOper QExpr | QCE QExpr
data QOper       = QL | QLE | QG | QGE | QE | QNE
data QExpr       = QInt Int | QString String
data QOrder      = Asc | Desc | NoOrder
data QLimit      = Limit Int | NoLimit
data QAs         = As String | DefaultAs

-- A Succes/Failure data type used for parsing
--
data E a = Ok a | Failed String

instance Functor E where
  fmap f (Ok a)  = Ok $ f a
  fmap f (Failed e) = Failed e

instance Monad E where
  return = Ok
  m >>= k = case m of { Ok a -> k a ; Failed e -> Failed e }
  fail = Failed

