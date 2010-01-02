{
module QueryParser

import QueryLexer
}

%name queryParser

%tokentype { Token      }
%error     { parseError }


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


SUBQUERIES : SUBQUERY                 { [$1]     }
	   | SUBQUERIES '*' SUBQUERY  { $1 ++ $3 }

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

CONS : CONSTRAINT                       { [$1]     }
     | CONS ',' CONSTRAINT              { $1 ++ $3 }
     |                                  { []       }

CONSTRAINT : TABLE OPERATOR EXPR        { Constraint $1 $2 $3 }

OPERATOR : '<'           { <  }
	 | '>'           { >  }
	 | '<='          { >= }
	 | '>='          { <= }
	 | '=='          { == }
	 | '!='          { /= }

EXPR : integer           { QInt $1 }

QPREFIX : asc            { Asc  }
	| desc           { Desc }
        |                { Empty }

{

type QQuery = ([QSubQuery], QPrefix)

data QSubQuery = SubQuery Bool QTable [QConstraint]

data QPrefix = Empty | Asc | Desc

data QTable = Ext | Lang | Proj | File | Year | Month | Day | Dow | Doy

data QConstraint = Constraint QTable (Ord a => a -> a -> Bool) QExpr

data QExpr = QInt Int 


parseError   :: [Token]  -> a
parseProgram :: String   -> QQuery
parseFile    :: FilePath -> IO QQuery

parseError _ = error "Parse error"
parseProgram = queryParser . alexScanTokens
parseFile f  = readFile f >>= return . queryParser
parseQuery = queryParser

}
