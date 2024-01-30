module Parser.State (
    ParserState(..),
    ParserError(..),
    currentTokenWithInfo,
    advanceParser,
    newParserState
) where

import Ast.TokenTypes

data ParserState = ParserState {
    tokensLeft :: [TokenWithInfo],
    previousToken :: Maybe TokenWithInfo,
    lastIfLabel :: Int,
    lastDoVarNum :: Int 
}
    deriving (Show)

data ParserError = 
      ParserErrorExpectedExpression TokenWithInfo 
    | ParserErrorExpectedToken TokenWithInfo Token 
    | ParserErrorExpectedKeyword TokenWithInfo String
    | ParserErrorExpectedIdentifier TokenWithInfo
    | ParserErrorExpectedInteger TokenWithInfo
    | ParserErrorExpectedBlock TokenWithInfo
    | ParserErrorExpectedIfConstruct TokenWithInfo
    | ParserErrorExpectedAssignment TokenWithInfo
    | ParserErrorUnexpectedToken TokenWithInfo
    | ParserErrorUnknown TokenWithInfo
    | ParserErrorDuplicateLabel TokenWithInfo Int
    | ParserErrorNotImplemented
    deriving(Show)

currentTokenWithInfo ParserState { tokensLeft=(twi:_) } = twi

advanceParser state@ParserState{ tokensLeft=(t:ts) } = state { 
    tokensLeft=ts,
    previousToken=Just(t)
}

newParserState tokens = ParserState { 
    tokensLeft=tokens, 
    previousToken=Nothing,
    lastIfLabel=0,
    lastDoVarNum=0
}

