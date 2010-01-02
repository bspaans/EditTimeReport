{
module QueryLexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$ident = [a-zA-Z0-9]

tokens :-

  $white+     ;
  "#".*       ;
  ">"         { \s -> TL                } 
  "<"         { \s -> TM                } 
  ">="        { \s -> TLE               } 
  "<="        { \s -> TME               } 
  "=="        { \s -> TEqual            } 
  "!="        { \s -> TNEqual           } 
  "/="        { \s -> TNEqual           } 
  "("         { \s -> TParenOpen        } 
  ")"         { \s -> TParenClose       } 
  ","         { \s -> TComma            }
  "*"         { \s -> TMultiply         }
  extension   { \s -> TExtension        } 
  language    { \s -> TLanguage         } 
  project     { \s -> TProject          } 
  filename    { \s -> TFilename         } 
  year        { \s -> TYear             } 
  month       { \s -> TMonth            } 
  day         { \s -> TDay              } 
  dow         { \s -> TDow              } 
  doy         { \s -> TDoy              } 
  ascending   { \s -> TAscending        } 
  descending  { \s -> TDescending       } 
  asc         { \s -> TAscending        }
  desc        { \s -> TDescending       }
  limit       { \s -> TLimit            }
  group       { \s -> TGroup            }
  $digit+     { \s -> TInteger (read s) } 
  
{
data ConstraintToken = TExtension
                     | TLanguage 
                     | TProject
                     | TFilename
                     | TYear
                     | TMonth
                     | TDay
                     | TDow
                     | TDoy
                     | TAscending
                     | TDescending
                     | TLimit
                     | TParenOpen
                     | TParenClose
                     | TComma
                     | TMultiply
                     | TEqual
                     | TNEqual
                     | TL          
                     | TM      
                     | TLE     
                     | TME 
                     | TInteger Int
                     deriving (Eq, Show)
}
