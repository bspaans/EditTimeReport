module QueryAST where

import Control.Monad.Trans

-- * AST for the Query DSL
--
type QCommands   = [QCommand]
type QCommand    = Either QQuery QAssign 
data QAssign     = QAssign String QQuery
type QQuery      = ([QSubQuery], Maybe String)
data QSubQuery   = QSubQuery Bool QIndex [QConstraint] QAs QOrder QLimit | QCall String
data QIndex      = Ext | Lang | Proj | Path | File | Dir | Year
                 | Month | Week | Day | Dow | Doy deriving (Eq, Show)
data QConstraint = QC QIndex QOper QExpr | QCOE QOper QExpr | QCE QExpr
data QOper       = QL | QLE | QG | QGE | QE | QNE | QREG
data QExpr       = QInt Int | QString String
data QOrder      = Asc | Desc | NoOrder
data QLimit      = Limit Int | NoLimit
data QAs         = As String | DefaultAs


-- A Succes/Failure data type used for parsing
--
data E a = Ok a | Failed String

newtype ET m a = ET { runET :: m (E a) }


instance Functor E where
  fmap f (Ok a)     = Ok $ f a
  fmap f (Failed e) = Failed e

instance Monad E where
  return = Ok
  m >>= k = case m of { Ok a -> k a ; Failed e -> Failed e }
  fail = Failed


instance Functor m => Functor (ET m) where 
  fmap f m = ET (fmap (fmap f) (runET m))

instance Monad m => Monad (ET m) where 
  return  = ET . return . Ok 
  m >>= f = ET (do o <- runET m 
                   case o of 
                     Ok a -> runET (f a) 
                     Failed s -> return (Failed s))

instance MonadTrans ET where 
  lift m = ET (m >>= \o -> return (Ok o))
