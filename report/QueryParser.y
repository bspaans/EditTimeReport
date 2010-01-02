{
module QueryParser where

import QueryLexer
}

%name queryParser

%tokentype { ConstraintToken }
%error     { parseError      }


%token 
  '>'       { TL          } 
  '<'       { TM          } 
  '<='      { TLE         } 
  '>='      { TME         } 
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

SUBQUERY : GROUP TABLE CONSTRAINTS { SubQuery $1 $2 $3 }

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

CONS : CONSTRAINT                       { [$1]       }
     | CONS ',' CONSTRAINT              { $1 ++ [$3] }
     |                                  { []         }

CONSTRAINT : TABLE OPERATOR EXPR        { Constraint $1 $2 $3 }

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

type QQuery = ([QSubQuery], QPrefix)

data QSubQuery = SubQuery Bool QTable [QConstraint]

data QPrefix = Empty | Asc | Desc

data QTable = Ext | Lang | Proj | File | Year | Month | Day | Dow | Doy

data QConstraint = Constraint QTable QOper QExpr

data QOper = QL | QLE | QG | QGE | QE | QNE

data QExpr = QInt Int 


parseError   :: [ConstraintToken]  -> a
parseQuery   :: String   -> QQuery
parseFile    :: FilePath -> IO QQuery

parseError _ = error "Parse error"
parseQuery   = queryParser . alexScanTokens
parseFile f  = readFile f >>= return . parseQuery

}
