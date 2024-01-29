module TokenTypes (
    Token(..),
    TokenWithInfo(..),
    TokenLocation(..),
    equivOperators,
    relOperators,
    addOperators,
    multOperators,
    unaryOperators,
    signOperators,
    newTokenLocation
) where

data Token = 
    TokenEqEq | TokenNeq | TokenLt | TokenLeq | TokenGt | TokenGeq |
    TokenEqv | TokenNeqv | 
    TokenAnd | TokenOr | 
    TokenNot |

    TokenLeftParen | TokenRightParen | TokenComma | TokenSemicolon | 
    TokenEq | 
    TokenPlus | TokenMinus | TokenStar | TokenSlash | TokenPow |

    TokenString String | TokenInteger Int | TokenFloat Double | TokenBool Bool | 
    TokenIdentifier String |

    TokenEof
    deriving (Show, Eq)

equivOperators = [TokenEqv, TokenNeqv, TokenEq]
relOperators = [TokenEqEq, TokenNeq, TokenLt, TokenLeq, TokenGt, TokenGeq]
addOperators = [TokenPlus, TokenMinus]
multOperators = [TokenStar, TokenSlash]
unaryOperators = [TokenNot, TokenPlus, TokenMinus]
signOperators  = [TokenPlus, TokenMinus]

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

