module Parser ( 
    expression,
    newParserState
) where

import AstTypes
import TokenTypes
import Utils
import Debug.Trace

data ParserState = ParserState {
    tokensLeft :: [TokenWithInfo],
    previousToken :: Maybe(TokenWithInfo)
}
    deriving (Show)

data ParserError = 
    ParserErrorExpectedExpression(TokenWithInfo) | 
    ParserErrorExpectedRightParen(TokenWithInfo) | 
    ParserErrorUnexpectedToken(TokenWithInfo) | 
    ParserErrorUnknown(TokenWithInfo) | 
    ParserErrorNotImplemented
    deriving(Show)

type ParseResult = (Either (ParserError) (Expr, ParserState))

newParserState tokens = ParserState { 
    tokensLeft=tokens, 
    previousToken=Nothing 
}

currentToken ParserState { tokensLeft=(TokenWithInfo{token=t}:_) } = t
currentTokenWithInfo ParserState { tokensLeft=(twi:_) } = twi

advanceParser ParserState{ tokensLeft=(t:ts) } = ParserState { 
    tokensLeft=ts,
    previousToken=Just(t)
}

type ParsingFunction = ParserState -> ParseResult

program, 
    executionPart, executionPartConstruct, executableConstruct,
    ifConstruct, elseIfStmt, elseStmt,
    doConstruct, loopControl, 
    block,
    actionStmt, 
    assignmentStmt, gotoStmt, ifStmt, computedGotoStmt,
    printStmt, readStmt, 
    expression, equivalence, equivOperand, 
    orOperand, 
    andOperand, level4Expr, level2Expr, addOperand, 
    multOperand, level1Expr, 
    primary :: ParsingFunction

tokenToBinaryOp :: Token -> Maybe(BinaryOp)
tokenToBinaryOp TokenEqEq   = Just(BinOpEq)
tokenToBinaryOp TokenNeq    = Just(BinOpNeq)
tokenToBinaryOp TokenEqv    = Just(BinOpEq)
tokenToBinaryOp TokenNeqv   = Just(BinOpNeq)
tokenToBinaryOp TokenGt     = Just(BinOpGt)
tokenToBinaryOp TokenGeq    = Just(BinOpGeq)
tokenToBinaryOp TokenLt     = Just(BinOpLt)
tokenToBinaryOp TokenLeq    = Just(BinOpLeq)
tokenToBinaryOp TokenPlus   = Just(BinOpAdd)
tokenToBinaryOp TokenMinus  = Just(BinOpSub)
tokenToBinaryOp TokenSlash  = Just(BinOpDiv)
tokenToBinaryOp TokenStar   = Just(BinOpMult)
tokenToBinaryOp TokenPow    = Just(BinOpPow)
tokenToBinaryOp TokenAnd    = Just(BinOpAnd)
tokenToBinaryOp TokenOr     = Just(BinOpOr)
tokenToBinaryOp _           = Nothing

tokenToUnaryOp :: Token -> Maybe(UnaryOp)
tokenToUnaryOp TokenNot     = Just(UnOpNot)
tokenToUnaryOp TokenMinus   = Just(UnOpMinus)
tokenToUnaryOp TokenPlus    = Just(UnOpPlus)
tokenToUnaryOp _            = Nothing

matchToken :: [Token] -> ParserState -> Maybe(ParserState)
matchToken tokens state@ParserState{ tokensLeft=(TokenWithInfo{token=t}:ts) }
    | (t `elem` tokens) = Just(advanceParser state)
    | otherwise         = Nothing

whileMatchesRight :: [Token] -> ParsingFunction -> (Expr, ParserState) -> ParseResult
whileMatchesRight tokens f orgResult@(orgExpr, orgState) =
    maybeOr
        (Right(orgResult))
        (\stateAfterMatch -> 
            (f stateAfterMatch) >>=
                \newResult -> 
                    (convertToBinaryExpr 
                        orgExpr 
                        (previousToken stateAfterMatch)
                        (whileMatchesRight tokens f newResult)))
        (matchToken tokens orgState)

convertToBinaryExpr :: Expr -> Maybe(TokenWithInfo) -> ParseResult -> ParseResult
convertToBinaryExpr _ _ result@(Left(_)) = result
convertToBinaryExpr _ (Nothing) (Right(expr, state)) = (Left(ParserErrorUnknown(currentTokenWithInfo(state))))
convertToBinaryExpr expr (Just(twi@TokenWithInfo{ token=token })) (Right(expr2, state)) =
    maybeOr 
        (Left(ParserErrorUnexpectedToken(twi)))
        (\t -> (Right(ExprBin(expr, t, expr2), state)))
        (tokenToBinaryOp token)

convertToUnaryExpr :: Maybe(TokenWithInfo) -> ParseResult -> ParseResult
convertToUnaryExpr _ result@(Left(_)) = result
convertToUnaryExpr (Nothing) (Right(expr, state)) = (Left(ParserErrorUnknown(currentTokenWithInfo(state))))
convertToUnaryExpr (Just(twi@TokenWithInfo{ token=token })) (Right(expr, state)) = 
    maybeOr
        (Left(ParserErrorUnexpectedToken(twi))) 
        (\t -> (Right(ExprUn(t, expr), state)))
        (tokenToUnaryOp token)

parseBinaryRightLoop :: ParsingFunction -> [Token] -> ParsingFunction -> ParserState -> ParseResult
parseBinaryRightLoop a operators b state =
    (a state) >>= whileMatchesRight operators b

parseBinaryLeftLoop :: ParsingFunction -> [Token] -> ParserState -> ParseResult
parseBinaryLeftLoop f operators state =
    (f state) >>=
        \result@(expr, newState) -> 
            maybeOr
                (Right(result))
                (\matchState -> 
                    convertToBinaryExpr 
                        expr 
                        (previousToken matchState) 
                        (parseBinaryLeftLoop f operators matchState))
                (matchToken operators newState)

parseUnaryLeftLoop :: ParsingFunction -> [Token] -> ParserState -> ParseResult
parseUnaryLeftLoop f tokens orgState =
    maybeOr
        res
        (\state -> 
            convertToUnaryExpr 
                (previousToken state)
                (parseUnaryLeftLoop f tokens state)
        )
        (matchToken tokens orgState)
    where res = (f orgState)

advanceResult :: ParseResult -> ParseResult
advanceResult = (flip (>>=)) (\(expr, state) -> Right(expr, (advanceParser state)))

-- expression parsers

expression      = equivalence
equivalence     = parseBinaryRightLoop equivOperand equivOperators equivOperand
equivOperand    = parseBinaryRightLoop orOperand [TokenOr] orOperand
orOperand       = parseBinaryRightLoop andOperand [TokenAnd] andOperand
andOperand      = parseUnaryLeftLoop level4Expr [TokenNot]
level4Expr      = parseBinaryLeftLoop level2Expr relOperators 
-- TODO addOperand loop (?)
level2Expr      = parseBinaryLeftLoop addOperand addOperators
addOperand      = parseBinaryRightLoop multOperand multOperators addOperand
multOperand     = parseBinaryRightLoop level1Expr [TokenPow] multOperand
level1Expr      = parseUnaryLeftLoop primary [TokenNot]
-- TODO calls

primary state@ParserState { tokensLeft=(twi@TokenWithInfo{token=t}:ts) } = 
    case t of
        TokenString(s) -> Right(ExprString(s), advanceParser state) 
        TokenInteger(i) -> Right(ExprInteger(i), advanceParser state)
        TokenFloat(f) -> Right(ExprFloat(f), advanceParser state)
        TokenBool(b) -> Right(ExprBool(b), advanceParser state)
        TokenIdentifier(i) -> Right(ExprIdentifier(i), advanceParser state)
        TokenLeftParen -> (expression $ advanceParser state) >>=
            \x -> case x of
                (expr,
                    state@ParserState{ 
                        tokensLeft=(TokenWithInfo{ token=TokenRightParen }:ts) 
                    }) -> Right(expr, advanceParser state)
                (expr,
                    state@ParserState{ 
                        tokensLeft=(t:ts) 
                    }) -> Left(ParserErrorExpectedRightParen(t))
        otherwise -> Left(ParserErrorExpectedExpression(twi))

-- statement parsers

program = expression
executionPart = expression
executionPartConstruct = expression
executableConstruct = expression
ifConstruct = expression
elseIfStmt = expression 
elseStmt = expression
doConstruct = expression 
loopControl = expression
block = expression
actionStmt = expression
assignmentStmt = expression
gotoStmt = expression
ifStmt = expression
computedGotoStmt = expression
printStmt = expression
readStmt = expression
