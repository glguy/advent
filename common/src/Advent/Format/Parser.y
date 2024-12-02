{
module Advent.Format.Parser (parseFormat, ParseError(..)) where

import Advent.Format.Types
import Advent.Format.Lexer (AlexPosn(..))

}

%tokentype                      { (AlexPosn, Token)     }

%token
'('                             { ($$, TOpenGroup)      }
')'                             { (_, TCloseGroup)      }
'*'                             { (_, TMany)            }
'+'                             { (_, TSome)            }
'&'                             { (_, TSepBy)           }
'|'                             { (_, TAlt)             }
'!'                             { (_, TBang)            }
'%a'                            { (_, TAnyLetter)       }
'%c'                            { (_, TAnyChar)         }
'%s'                            { (_, TAnyWord)         }
'%u'                            { (_, TUnsignedInt)     }
'%d'                            { (_, TSignedInt)       }
'%x'                            { (_, THexInt)          }
'%lu'                           { (_, TUnsignedInteger) }
'%ld'                           { (_, TSignedInteger)   }
'%lx'                           { (_, THexInteger)      }
LIT                             { (_, TLiteral $$)      }
NAME                            { (_, TAt $$)           }

%name parseFormat format

%monad                          { Either ParseError     }
%error                          { parseError            }

%left '&' '*' '+' '!'

%%

format ::                       { Format                }
  : atoms                       { $1                    }
  | format '|' atoms            { Alt $1 $3             }

atoms ::                        { Format                }
  :                             { Follow []             }
  | atoms atom                  { Follow [$1, $2]       }

atom ::                         { Format                }
  : '(' format ')'              { Group $2              }
  | '(' format error            {% Left (Unclosed $1)   }
  | '%u'                        { UnsignedInt           }
  | '%d'                        { SignedInt             }
  | '%x'                        { HexInt                }
  | '%lu'                       { UnsignedInteger       }
  | '%ld'                       { SignedInteger         }
  | '%lx'                       { HexInteger            }
  | '%s'                        { Word                  }
  | '%c'                        { Char                  }
  | '%a'                        { Letter                }
  | LIT                         { Literal [$1]          }
  | atom '*'                    { Many $1               }
  | atom '+'                    { Some $1               }
  | atom '!'                    { Gather $1             }
  | atom '&' atom               { SepBy $1 $3           }
  | NAME                        { Named $1              }

{
data ParseError
  = Unclosed AlexPosn
  | UnexpectedEOF
  | UnexpectedToken AlexPosn Token
  deriving Show

parseError :: [(AlexPosn, Token)] -> Either ParseError a
parseError ((p,t):_) = Left (UnexpectedToken p t)
parseError [] = Left UnexpectedEOF
}
