module TokenTypes (
    Token(..),
    TokenWithInfo(..),
    TokenLocation(..),
    newTokenLocation
) where

data Token = 
    TokenEqEq | TokenNeq | TokenLt | TokenLeq | TokenGt | TokenGeq |
    TokenAnd | TokenOr | 

    TokenLeftParen | TokenRightParen | TokenComma | TokenSemicolon | 
    TokenEq | 
    TokenPlus | TokenMinus | TokenStar | TokenSlash | TokenPow |

    TokenString(String) | TokenInteger(Integer) | TokenFloat(Double) | TokenBool(Bool) | 
    TokenIdentifier(String) |

    TokenEof
    deriving (Show)

data TokenLocation = TokenLocation {
    tokenPos :: Int,
    tokenLine :: Int
}
    deriving (Show)

newTokenLocation p l = TokenLocation{ tokenPos=p, tokenLine=l }

data TokenWithInfo = TokenWithInfo { 
    token :: Token,
    tokenLocation :: TokenLocation
}
    deriving (Show)

