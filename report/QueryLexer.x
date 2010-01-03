{
module QueryLexer where

import Data.Either
}


$digit = 0-9
$alpha = [a-zA-Z]
$ident = [a-zA-Z0-9]

tokens :-

  $white+     ;
  "#".*       ;
  ">"         { \s -> TL                } 
  "<"         { \s -> TG                } 
  "<="        { \s -> TLE               } 
  ">="        { \s -> TGE               } 
  "=="        { \s -> TEqual            } 
  "!="        { \s -> TNEqual           } 
  "/="        { \s -> TNEqual           } 
  "("         { \s -> TParenOpen        } 
  ")"         { \s -> TParenClose       } 
  ","         { \s -> TComma            }
  "*"         { \s -> TProduct          }
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
                     | TGroup
                     | TAscending
                     | TDescending
                     | TLimit
                     | TParenOpen
                     | TParenClose
                     | TComma
                     | TProduct
                     | TEqual
                     | TNEqual
                     | TL          
                     | TG      
                     | TLE     
                     | TGE 
                     | TInteger Int
                     deriving (Eq, Show)

type AlexInput = (Char,     -- previous char
                  String)   -- current input string

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (_, [])   = Nothing
alexGetChar (_, c:cs) = Just (c, (c,cs))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c,_) = c

alexScanTokens :: String -> Either String [ConstraintToken]
alexScanTokens str = case go ('\n',str) of
                          [] -> Right []
                          cs -> case last cs of 
                                  Left a -> Left a
                                  _ -> Right (rights cs)
  where go inp@(_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> [Left "lexical error"]
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> Right (act (take len str)): go inp'

}
