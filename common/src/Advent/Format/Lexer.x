{
module Advent.Format.Lexer where

import Advent.Format.Types
}
%wrapper "posn"

tokens :-

"("     { token_ TOpenGroup             }
")"     { token_ TCloseGroup            }
"%c"    { token_ TAnyChar               }
"%a"    { token_ TAnyLetter             }
"%s"    { token_ TAnyWord               }
"%t"    { token_ (TLiteral '\t')        }
"%u"    { token_ TUnsignedInt           }
"%d"    { token_ TSignedInt             }
"%x"    { token_ THexInt                }
"%lu"   { token_ TUnsignedInteger       }
"%ld"   { token_ TSignedInteger         }
"%lx"   { token_ THexInteger            }
"*"     { token_ TMany                  }
"+"     { token_ TSome                  }
"&"     { token_ TSepBy                 }
"|"     { token_ TAlt                   }
"!"     { token_ TBang                  }
"@" .   { token (TAt . tail)            }
"%n"    { token_ (TLiteral '\n')        }
"%" .   { token (TLiteral . head . tail)}
.       { token (TLiteral . head)       }

{
type Action = AlexPosn -> String -> (AlexPosn, Token)

token_ :: Token -> Action
token_ x p _ = (p, x)

token :: (String -> Token) -> Action
token f p str = (p, f str)
}
