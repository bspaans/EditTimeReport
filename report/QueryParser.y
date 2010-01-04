{
module QueryParser (parseQuery) where

import QueryLexer
import QueryAST

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
  string    { TString p $$  }

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
        |                   { [] } 

CONS : CONSTRAINT                       { [$1]       }
     | CONS ',' CONSTRAINT              { $1 ++ [$3] }
     |                                  { []         }

CONSTRAINT : INDEX OPERATOR EXPR        {% if elem $1 [Year, Day, Doy] then case $3 of 
                                               QInt _ -> returnE $ QC $1 $2 $3
                                               QString s -> failE $ "Expecting an integer, but got string \"" ++ s ++ "\"" 
                                           else (returnE $ QC $1 $2 $3) }

OPERATOR : '<'           { QL  }
         | '>'           { QG  }
         | '<='          { QLE }
         | '>='          { QGE }
         | '=='          { QE  }
         | '!='          { QNE }

EXPR : integer           { QInt $1 }
     | string            { QString $1 }

ORDER : asc           { Asc     }
      | desc          { Desc    }
      |               { NoOrder }

LIMIT: limit integer  { Limit $2 }
     |                { NoLimit  }

{

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
