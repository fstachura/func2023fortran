module Parser where

import AstTypes
import TokenTypes

data ParserState = ParserState {
    tokens :: [TokenWithInfo],
    previousToken :: TokenWithInfo
}



