{
module QueryLexer where

import Data.Either
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$ident = [a-zA-Z0-9]
$s     = [^'"']

tokens :-

  $white+     ;
  "#".*       ;
  ">"         { \p s -> TL p                      } 
  "<"         { \p s -> TG p                      } 
  "<="        { \p s -> TLE p                     } 
  ">="        { \p s -> TGE p                     } 
  "=="        { \p s -> TEqual p                  } 
  "="         { \p s -> TEqual p                  }
  "!="        { \p s -> TNEqual p                 } 
  "/="        { \p s -> TNEqual p                 } 
  "("         { \p s -> TParenOpen p              } 
  ")"         { \p s -> TParenClose p             } 
  ","         { \p s -> TComma p                  } 
  "*"         { \p s -> TProduct p                } 
  extension   { \p s -> TExtension p              } 
  language    { \p s -> TLanguage p               } 
  project     { \p s -> TProject p                } 
  filename    { \p s -> TFilename p               } 
  year        { \p s -> TYear p                   } 
  month       { \p s -> TMonth p                  } 
  day         { \p s -> TDay p                    } 
  dow         { \p s -> TDow p                    } 
  doy         { \p s -> TDoy p                    } 
  ascending   { \p s -> TAscending p              } 
  descending  { \p s -> TDescending p             } 
  asc         { \p s -> TAscending p              } 
  desc        { \p s -> TDescending p             } 
  limit       { \p s -> TLimit p                  } 
  group       { \p s -> TGroup p                  } 
  $digit+     { \p s -> TInteger p (read s)       } 
  \"$s*\"     { \p s -> TString p (init (tail s)) } 
  
{
data ConstraintToken = TExtension AlexPosn
                     | TLanguage AlexPosn
                     | TProject AlexPosn
                     | TFilename AlexPosn
                     | TYear AlexPosn
                     | TMonth AlexPosn
                     | TDay AlexPosn
                     | TDow AlexPosn
                     | TDoy AlexPosn
                     | TGroup AlexPosn
                     | TAscending AlexPosn
                     | TDescending AlexPosn
                     | TLimit AlexPosn
                     | TParenOpen AlexPosn
                     | TParenClose AlexPosn
                     | TComma AlexPosn
                     | TProduct AlexPosn
                     | TEqual AlexPosn
                     | TNEqual AlexPosn
                     | TL AlexPosn
                     | TG AlexPosn
                     | TLE AlexPosn
                     | TGE AlexPosn
                     | TInteger AlexPosn Int
                     | TString AlexPosn String
                     deriving (Eq, Show)


alexScanTokens' :: String -> Either String [ConstraintToken]
alexScanTokens' str = case go (alexStartPos, '\n', str) of
                          [] -> Right []
                          cs -> case last cs of 
                                  Left a -> Left a
                                  _      -> Right (rights cs)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> [Left $ "Lexical error on " ++ showPos pos
                                        ++ if null str then "."
                                             else ". Not a valid token: '" ++ (head . words $ str) ++ "'"]
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> Right (act pos (take len str)): go inp'

showPos (AlexPn _ li co) = unwords ["line", show li, ", column", show co]

showTokenPos (TExtension p)  = showPos p
showTokenPos (TLanguage p)   = showPos p
showTokenPos (TProject p)    = showPos p
showTokenPos (TFilename p)   = showPos p
showTokenPos (TYear p)       = showPos p
showTokenPos (TMonth p)      = showPos p
showTokenPos (TDay p)        = showPos p
showTokenPos (TDow p)        = showPos p
showTokenPos (TDoy p)        = showPos p
showTokenPos (TGroup p)      = showPos p
showTokenPos (TAscending p)  = showPos p
showTokenPos (TDescending p) = showPos p
showTokenPos (TLimit p)      = showPos p
showTokenPos (TParenOpen p)  = showPos p
showTokenPos (TParenClose p) = showPos p
showTokenPos (TComma p)      = showPos p
showTokenPos (TProduct p)    = showPos p
showTokenPos (TEqual p)      = showPos p
showTokenPos (TNEqual p)     = showPos p
showTokenPos (TL p)          = showPos p
showTokenPos (TG p)          = showPos p
showTokenPos (TLE p)         = showPos p
showTokenPos (TGE p)         = showPos p
showTokenPos (TInteger p _)  = showPos p
showTokenPos (TString p _)   = showPos p
}
