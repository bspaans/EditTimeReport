{
module QueryLexer where

import Data.Either
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$upper = A-Z
$ident = [a-zA-Z0-9]
$s     = [^'"']

tokens :-

  $white+       ;
  "#".*         ;
  ">"           { \p s -> TL p                      }
  "<"           { \p s -> TG p                      }
  "<="          { \p s -> TLE p                     }
  ">="          { \p s -> TGE p                     }
  "=="          { \p s -> TEqual p                  }
  "="           { \p s -> TEqual p                  }
  "!="          { \p s -> TNEqual p                 }
  "/="          { \p s -> TNEqual p                 }
  "("           { \p s -> TParenOpen p              }
  ")"           { \p s -> TParenClose p             }
  ","           { \p s -> TComma p                  }
  "*"           { \p s -> TProduct p                }
  ":="          { \p s -> TAssign p                 }
  ";"           { \p s -> TSemiColon p              }
  "~="          { \p s -> TRegex p                  }
  extension     { \p s -> TExtension p              }
  extensions    { \p s -> TExtension p              }
  languages     { \p s -> TLanguage p               }
  language      { \p s -> TLanguage p               }
  projects      { \p s -> TProject p                }
  project       { \p s -> TProject p                }
  proj          { \p s -> TProject p                }
  path          { \p s -> TPath p                   }
  filenames     { \p s -> TFilename p               } 
  filename      { \p s -> TFilename p               } 
  file          { \p s -> TFilename p               } 
  directories   { \p s -> TDirectory p              } 
  directory     { \p s -> TDirectory p              } 
  years         { \p s -> TYear p                   }
  year          { \p s -> TYear p                   }
  months        { \p s -> TMonth p                  }
  month         { \p s -> TMonth p                  }
  weeks         { \p s -> TWeek p                   }
  week          { \p s -> TWeek p                   }
  days          { \p s -> TDay p                    }
  day           { \p s -> TDay p                    }
  dow           { \p s -> TDow p                    }
  doy           { \p s -> TDoy p                    }
  ascending     { \p s -> TAscending p              }
  descending    { \p s -> TDescending p             }
  asc           { \p s -> TAscending p              }
  desc          { \p s -> TDescending p             }
  limit         { \p s -> TLimit p                  }
  as            { \p s -> TAs p                     }
  group         { \p s -> TGroup p                  }
  "&"           { \p s -> TGroup p                  }
  nogrouping    { \p s -> TDontGroup p              }
  nogroup       { \p s -> TDontGroup p              }
  "!"           { \p s -> TDontGroup p              }
  titled        { \p s -> TTitled p s               }
  $digit+       { \p s -> TInteger p (read s)       }
  $upper$ident* { \p s -> TIdent p s                }
  \"$s*\"       { \p s -> TString p (init (tail s)) }
  
{
data ConstraintToken = TExtension AlexPosn
                     | TLanguage AlexPosn
                     | TProject AlexPosn
                     | TPath AlexPosn
                     | TFilename AlexPosn 
                     | TDirectory AlexPosn
                     | TYear AlexPosn
                     | TMonth AlexPosn
                     | TWeek AlexPosn
                     | TDay AlexPosn
                     | TDow AlexPosn
                     | TDoy AlexPosn
                     | TGroup AlexPosn
                     | TDontGroup AlexPosn
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
                     | TRegex AlexPosn
                     | TSemiColon AlexPosn
                     | TAs AlexPosn
                     | TInteger AlexPosn Int
                     | TIdent AlexPosn String
                     | TAssign AlexPosn
                     | TString AlexPosn String
                     | TTitled AlexPosn String
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
showTokenPos (TPath p)       = showPos p
showTokenPos (TFilename p)   = showPos p
showTokenPos (TDirectory p)  = showPos p
showTokenPos (TYear p)       = showPos p
showTokenPos (TWeek p)       = showPos p
showTokenPos (TMonth p)      = showPos p
showTokenPos (TDay p)        = showPos p
showTokenPos (TDow p)        = showPos p
showTokenPos (TDoy p)        = showPos p
showTokenPos (TGroup p)      = showPos p
showTokenPos (TDontGroup p)  = showPos p
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
showTokenPos (TRegex p)      = showPos p
showTokenPos (TInteger p _)  = showPos p
showTokenPos (TString p _)   = showPos p
showTokenPos (TAssign p)     = showPos p
showTokenPos (TIdent p _)    = showPos p
showTokenPos (TSemiColon p)  = showPos p
showTokenPos (TAs p)         = showPos p
showTokenPos (TTitled p _)   = showPos p
}
