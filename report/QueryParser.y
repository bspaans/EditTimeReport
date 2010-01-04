{
module QueryParser (QQuery, QConstraint(..), QSubQuery(..)
                   , QOper(..), QExpr(..), QOrder(..), QLimit(..)
                   , QIndex(..), parseQuery, E(..)
                   ) where

import QueryLexer
}

%name queryParser

%tokentype  { ConstraintToken }
%error      { parseError      }

%monad      { E } { thenE } { returnE }

%token 
  '>'       { TL p          } 
  '<'       { TG p          } 
  '<='      { TLE p         } 
  '>='      { TGE p         } 
  '!='      { TNEqual p     } 
  '=='      { TEqual p      } 
  '('       { TParenOpen p  } 
  ')'       { TParenClose p } 
  ','       { TComma p      } 
  '*'       { TProduct p    } 
  extension { TExtension p  } 
  language  { TLanguage p   } 
  project   { TProject p    } 
  filename  { TFilename p   } 
  year      { TYear p       } 
  month     { TMonth p      } 
  day       { TDay p        } 
  dow       { TDow p        } 
  doy       { TDoy p        } 
  group     { TGroup p      } 
  limit     { TLimit p      } 
  asc       { TAscending p  } 
  desc      { TDescending p } 
  integer   { TInteger p $$ } 


%%


QUERY : SUBQUERIES ORDER LIMIT { ($1, $2, $3)           } 
      |                        { ([], NoOrder, NoLimit) }


SUBQUERIES : SUBQUERY                 { [$1]        }
           | SUBQUERIES '*' SUBQUERY  { $1 ++ [$3]  }

SUBQUERY : GROUP INDEX CONSTRAINTS { QSubQuery $1 $2 $3 }

GROUP : group             { True  }
      |                   { False }

INDEX : extension         { Ext   }
      | language          { Lang  } 
      | project           { Proj  } 
      | filename          { File  } 
      | year              { Year  } 
      | month             { Month } 
      | day               { Day   } 
      | dow               { Dow   } 
      | doy               { Doy   } 
     
CONSTRAINTS : '(' CONS ')'  { $2 } 
        |               { [] } 

CONS : CONSTRAINT                       { [$1]       }
     | CONS ',' CONSTRAINT              { $1 ++ [$3] }
     |                                  { []         }

CONSTRAINT : INDEX OPERATOR EXPR        { QConstraint $1 $2 $3 }

OPERATOR : '<'           { QL  }
         | '>'           { QG  }
         | '<='          { QLE }
         | '>='          { QGE }
         | '=='          { QE  }
         | '!='          { QNE }

EXPR : integer           { QInt $1 }

ORDER : asc           { Asc     }
      | desc          { Desc    }
      |               { NoOrder }

LIMIT: limit integer  { Limit $2 }
     |                { NoLimit  }
{

type QQuery = ([QSubQuery], QOrder, QLimit)
data QSubQuery = QSubQuery Bool QIndex [QConstraint]
data QOrder = Asc | Desc | NoOrder
data QLimit = Limit Int | NoLimit 
data QIndex = Ext | Lang | Proj | File | Year | Month | Day | Dow | Doy
data QConstraint = QConstraint QIndex QOper QExpr
data QOper = QL | QLE | QG | QGE | QE | QNE
data QExpr = QInt Int 



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



parseError   :: [ConstraintToken]  -> E a
parseQuery   :: String   -> E QQuery
parseFile    :: FilePath -> IO (E QQuery)

parseError s = failE $ "Parse error on " ++  if null s then "<eof>"
                       else showTokenPos (head s)
parseQuery s = case alexScanTokens' s of 
                 Left err -> failE err
                 Right a -> queryParser a
parseFile f  = readFile f >>= return . parseQuery

}
