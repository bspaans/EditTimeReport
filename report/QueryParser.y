{
module QueryParser

import QueryLexer
}

%name constraintParser

%tokentype { Token      }
%error     { parseError }


%token 
  '>'       { TL          } 
  '<'       { TM          } 
  '<=       { TLE         } 
  '>='      { TME         } 
  '!='      { TNEqual     } 
  '=='      { TEqual      } 
  '('       { TParenOpen  } 
  ')'       { TParenClose } 
  ','       { TComma      } 
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


QUERY : SUBQUERIES         { }
      | SUBQUERIES QPREFIX { } 
      |                    { }


SUBQUERIES : SUBQUERY
	   | SUBQUERIES '*' SUBQUERY

SUBQUERY : GROUP TABLE CONSTRAINTS

GROUP : group             { True  }
      |                   { False }

TABLE : extension         { }
      | language          { } 
      | project           { }
      | filename          { }
      | year              { }
      | month             { } 
      | day               { }
      | dow               { } 
      | doy               { }
     
CONSTRAINTS : '(' CONS ')'  { $2 } 

CONS : CONSTRAINT | CONS ',' CONSTRAINT {    }
     |                                  { [] }

CONSTRAINT : TABLE OPERATOR EXPR

OPERATOR : '<'           { }
	 | '>'           { }
	 | '<='          { }
	 | '>='          { }
	 | '=='          { }
	 | '!='          { }

EXPR : integer           { $1 }

QPREFIX : asc            { }
	| desc           { }

{

parseError   :: [Token]  -> a
parseProgram :: String   -> Program 
parseFile    :: FilePath -> IO Program

parseError _ = error "Parse error"
parseProgram = constraintParser . alexScanTokens
parseFile f  = readFile f >>= return . parseProgram

}
