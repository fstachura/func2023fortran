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
    newTokenLocation,
    dotTokenMap,
    identifierChars, 
    simpleTokenMap 
) where

import Utils

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

identifierChars = letters ++ digits ++ "_"

dotTokenMap = [
        ("and", TokenAnd),
        ("or", TokenOr),
        ("not", TokenNot), 
        ("eq", TokenEq), 
        ("ne", TokenNeq),
        ("eqv", TokenEqv),
        ("neqv", TokenNeqv),
        ("lt", TokenLt),
        ("le", TokenLeq),
        ("gt", TokenGt),
        ("ge", TokenGeq),
        ("true", TokenBool True),
        ("t", TokenBool True),
        ("false", TokenBool False),
        ("f", TokenBool False)
    ]

simpleTokenMap = [
        ("<=", TokenLeq),
        (">=", TokenGeq),
        ("==", TokenEqEq),
        ("/=", TokenNeq),
        ("!=", TokenNeq),
        ("**", TokenPow),
        ("+", TokenPlus),
        ("-", TokenMinus),
        ("*", TokenStar),
        ("/", TokenSlash),
        ("=", TokenEq),
        (">", TokenGt),
        ("<", TokenLt),
        ("(", TokenLeftParen),
        (")", TokenRightParen),
        (",", TokenComma),
        (";", TokenSemicolon)
    ]

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

