{
module QueryParser (ParseResult, parseCommands, parseFile) where

import Prelude hiding (catch)
import qualified Control.Exception as C

import QueryLexer
import QueryAST

}

%name queryParser

%tokentype  { ConstraintToken }
%error      { parseError      }

%monad      { E } 

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
  ':='      { TAssign p     }
  ';'       { TSemiColon p  }
  as        { TAs p         }
  extension { TExtension p  } 
  language  { TLanguage p   } 
  project   { TProject p    } 
  path      { TPath p       } 
  filename  { TFilename p   }
  directory { TDirectory p  }
  year      { TYear p       } 
  month     { TMonth p      } 
  week      { TWeek p       }
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
  ident     { TIdent p $$   }
  titled    { TTitled p $$  } 

%%


COMMANDS : MULTICOMMAND         { $1 }
         | MULTICOMMAND COMMAND { $1 ++ [$2] }
         | COMMAND              { [$1] }

MULTICOMMAND: MULTICOMMAND SINGLECOMMAND  { $1 ++ [$2] } 
            | SINGLECOMMAND               { [$1]       }

SINGLECOMMAND : COMMAND ';' { $1 }


COMMAND : QUERY               { Left ($1, Nothing)  } 
	| QUERY titled string { Left ($1, Just $3)  }
        | ASSIGNMENT          { Right $1            }


ASSIGNMENT : ident ':=' QUERY { QAssign $1 ($3, Nothing) }

QUERY : SUBQUERY            { [$1]        }
      | QUERY '*' SUBQUERY  { $1 ++ [$3]  }

SUBQUERY : GROUP INDEX CONSTRAINTS AS ORDER LIMIT {% if typeCheckConstraints $2 $3 
                                        then return $ QSubQuery $1 $2 $3 $4 $5 $6
                                        else fail "Parse error in constraints: expecting an integer"}
         | ident                               { QCall $1 }

GROUP : group             { True  }
      | nogroup           { False }
      |                   { True  }

INDEX : extension         { Ext   } 
      | language          { Lang  } 
      | project           { Proj  } 
      | path              { Path  } 
      | filename          { File  } 
      | directory         { Dir   }
      | year              { Year  }  
      | month             { Month } 
      | week              { Week  }
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

AS : as string { As $2 }
   |           { DefaultAs }

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
                            QInt _ -> return $ QC a b c
                            QString s -> fail $ "Expecting an integer, but got string \"" ++ s ++ "\"" 
                      else (return $ QC a b c)

type ParseResult = E QCommands

parseError    :: [ConstraintToken]  -> E a
parseCommands :: String   -> ParseResult
parseFile     :: FilePath -> IO ParseResult

parseError    s = fail "Parse error."
parseCommands s = case alexScanTokens' s of 
                    Left err -> fail err
                    Right a -> queryParser a
parseFile     f = readFile f >>= return . parseCommands

}
