module Parser.Utils (
    tokenToBinaryOp,
    tokenToUnaryOp,
    matchToken,
    matchTokenOrFail,
    matchIdentifier,
    matchIdentifierOrFail,
    matchList,
    matchInteger,
    matchIntegerOrFail,
    matchKeyword,
    matchKeywordOrFail,
    skipSemicolons
) where

import Ast.TokenTypes
import Ast.AstTypes
import Utils.Utils
import Parser.State

binaryOpTokenMap = [
        (TokenEqEq, BinOpEq),
        (TokenEq, BinOpEq),
        (TokenNeq, BinOpNeq),
        (TokenEqv, BinOpEq),
        (TokenNeqv, BinOpNeq),
        (TokenGt, BinOpGt),
        (TokenGeq, BinOpGeq),
        (TokenLt, BinOpLt),
        (TokenLeq, BinOpLeq),
        (TokenPlus, BinOpAdd),
        (TokenMinus, BinOpSub),
        (TokenSlash, BinOpDiv),
        (TokenStar, BinOpMult),
        (TokenPow, BinOpPow),
        (TokenAnd, BinOpAnd),
        (TokenOr, BinOpOr)
    ]

unaryOpTokenMap = [
        (TokenNot, UnOpNot),
        (TokenMinus, UnOpMinus),
        (TokenPlus, UnOpPlus)
    ]

tokenToBinaryOp :: Token -> Maybe(BinaryOp)
tokenToBinaryOp = (flip lookup) binaryOpTokenMap

tokenToUnaryOp :: Token -> Maybe(UnaryOp)
tokenToUnaryOp = (flip lookup) unaryOpTokenMap

matchToken :: [Token] -> ParserState -> Maybe ParserState
matchToken tokens state@ParserState{ tokensLeft=(TokenWithInfo{token=t}:_) }
    | (t `elem` tokens) = Just $ advanceParser state
    | otherwise         = Nothing
matchToken _ _          = Nothing

matchTokenOrFail :: Token -> ParserState -> Either ParserError ParserState
matchTokenOrFail token state = 
    maybeOr
        (Left $ ParserErrorExpectedToken (currentTokenWithInfo state) token)
        (Right . id)
        (matchToken [token] state) 

matchIdentifier :: ParserState -> Maybe (String, ParserState)
matchIdentifier state@ParserState{ tokensLeft=(TokenWithInfo{token=(TokenIdentifier(is))}:_) } = 
    (Just(is, (advanceParser state)))
matchIdentifier _ = Nothing

matchIdentifierOrFail :: ParserState -> Either ParserError (String, ParserState)
matchIdentifierOrFail state = 
    maybeOr
        (Left $ ParserErrorExpectedIdentifier $ currentTokenWithInfo state)
        (Right . id)
        (matchIdentifier state)

matchList :: (ParserState -> Either ParserError (a, ParserState)) 
            -> ParserState -> Either ParserError ([a], ParserState)

matchList f orgState =
    (f orgState) >>=
    \(el, postElState) ->
        maybeOr
            (Right([el], postElState))
            (\postCommaState -> 
                (matchList f postCommaState) >>=
                \(elList, postElListState) ->
                    (Right(el:elList, postElListState)))
            (matchToken [TokenComma] postElState)

matchKeywordWithoutAdvance :: String -> ParserState -> Maybe ParserState
matchKeywordWithoutAdvance s state@ParserState{ tokensLeft=(TokenWithInfo{token=(TokenIdentifier(is))}:_) }
    | ((strToLower s) == (strToLower is)) = Just state
    | otherwise                           = Nothing
matchKeywordWithoutAdvance _ _ = Nothing

matchKeyword :: String -> ParserState -> Maybe ParserState
matchKeyword s state = (matchKeywordWithoutAdvance s state) >>= (Just . advanceParser)

matchKeywordOrFail :: String -> ParserState -> Either ParserError ParserState
matchKeywordOrFail keyword state = 
    maybeOr
        (Left $ ParserErrorExpectedKeyword (currentTokenWithInfo state) keyword)
        (Right . id)
        (matchKeyword keyword state)

matchInteger :: ParserState -> Maybe (Int, ParserState)
matchInteger state@ParserState{ tokensLeft=(TokenWithInfo{token=(TokenInteger(i))}:_) } = 
    (Just(i, (advanceParser state)))
matchInteger _ = Nothing

matchIntegerOrFail :: ParserState -> Either ParserError (Int, ParserState)
matchIntegerOrFail state = 
    maybeOr
        (Left $ ParserErrorExpectedInteger $ currentTokenWithInfo state)
        (Right . id)
        (matchInteger state)

skipSemicolons :: ParserState -> ParserState
skipSemicolons state =
    maybeOr
        state
        (skipSemicolons)
        (matchToken [TokenSemicolon] state)

