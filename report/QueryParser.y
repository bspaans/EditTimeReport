{
module QueryParser (QQuery, QConstraint(..), QSubQuery(..)
                   , QOper(..), QExpr(..)
                   , QTable(..), parseQuery, E(..)
                   ) where

import QueryLexer
}

%name queryParser

%tokentype  { ConstraintToken }
%error      { parseError      }

%monad      { E } { thenE } { returnE }

%token 
  '>'       { TL          } 
  '<'       { TG          } 
  '<='      { TLE         } 
  '>='      { TGE         } 
  '!='      { TNEqual     } 
  '=='      { TEqual      } 
  '('       { TParenOpen  } 
  ')'       { TParenClose } 
  ','       { TComma      } 
  '*'       { TProduct    }
  extension { TExtension  } 
  language  { TLanguage   } 
  project   { TProject    } 
  filename  { TFilename   } 
  year      { TYear       } 
  month     { TMonth      } 
  day       { TDay        } 
  dow       { TDow        } 
  doy       { TDoy        } 
  group     { TGroup      } 
  limit     { TLimit      } 
  asc       { TAscending  } 
  desc      { TDescending } 
  integer   { TInteger $$ } 


%%


QUERY : SUBQUERIES QPREFIX { ($1, $2)    } 
      |                    { ([], Empty) }


SUBQUERIES : SUBQUERY                 { [$1]        }
           | SUBQUERIES '*' SUBQUERY  { $1 ++ [$3]  }

SUBQUERY : GROUP TABLE CONSTRAINTS { QSubQuery $1 $2 $3 }

GROUP : group             { True  }
      |                   { False }

TABLE : extension         { Ext   }
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

CONSTRAINT : TABLE OPERATOR EXPR        { QConstraint $1 $2 $3 }

OPERATOR : '<'           { QL  }
         | '>'           { QG  }
         | '<='          { QLE }
         | '>='          { QGE }
         | '=='          { QE  }
         | '!='          { QNE }

EXPR : integer           { QInt $1 }

QPREFIX : asc            { Asc  }
        | desc           { Desc }
        |                { Empty }

{

type QQuery = ([QSubQuery], QPostfix)

data QSubQuery = QSubQuery Bool QTable [QConstraint]

data QPostfix = Empty | Asc | Desc | Limit Int

data QTable = Ext | Lang | Proj | File | Year | Month | Day | Dow | Doy

data QConstraint = QConstraint QTable QOper QExpr

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

parseError _ = failE "Parse error"
parseQuery   = queryParser . alexScanTokens
parseFile f  = readFile f >>= return . parseQuery

}
