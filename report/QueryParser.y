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
  nogroup   { TDontGroup p  }
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

SUBQUERY : GROUP INDEX CONSTRAINTS {% if typeCheckConstraints $2 $3 
                                        then returnE $ QSubQuery $1 $2 $3
                                        else failE "Parse error in constraints: expecting an integer"}

GROUP : group             { True  }
      | nogroup           { False }
      |                   { True  }

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
        | CONS              { $1 }
        |                   { [] }

CONS : CONSTRAINT                       { [$1]       }
     | CONS ',' CONSTRAINT              { $1 ++ [$3] }

CONSTRAINT : INDEX OPERATOR EXPR        {% typeCheckQC $1 $2 $3 }
           | OPERATOR EXPR              { QCOE $1 $2 }
           | EXPR                       { QCE $1 } 

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

typeCheckConstraints i cs = all typeCheck cs
  where typeCheck (QC _ _ _) = True
        typeCheck (QCOE _ e) = check e
        typeCheck (QCE e) = check e
        check e = if elem i [Year, Day, Doy] 
                    then case e of { QInt _ -> True ; _ -> False }
                    else True
                            

typeCheckQC a b c = if elem a [Year, Day, Doy] 
                      then case c of 
                            QInt _ -> returnE $ QC a b c
                            QString s -> failE $ "Expecting an integer, but got string \"" ++ s ++ "\"" 
                      else (returnE $ QC a b c)
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
