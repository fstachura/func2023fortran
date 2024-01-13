module Parser ( 
    expression,
    program,
    newParserState
) where

import AstTypes
import TokenTypes
import Utils
import Map
import Debug.Trace

data ParserState = ParserState {
    tokensLeft :: [TokenWithInfo],
    previousToken :: Maybe(TokenWithInfo),
    lastIfLabel :: Integer
}
    deriving (Show)

data ParserError = 
    ParserErrorExpectedExpression(TokenWithInfo) | 
    ParserErrorExpectedToken(TokenWithInfo, Token) | 
    ParserErrorExpectedKeyword(TokenWithInfo, String) | 
    ParserErrorExpectedIdentifier(TokenWithInfo) | 
    ParserErrorExpectedInteger(TokenWithInfo) | 
    ParserErrorExpectedBlock(TokenWithInfo) | 
    ParserErrorExpectedIfConstruct(TokenWithInfo) | 
    ParserErrorUnexpectedToken(TokenWithInfo) | 
    ParserErrorUnknown(TokenWithInfo) | 
    ParserErrorDuplicateLabel(TokenWithInfo, Integer) |
    ParserErrorNotImplemented
    deriving(Show)

type ExprParseResult                = (Either (ParserError) (Expr, ParserState))
type StmtParseResult                = (Either (ParserError) (Stmt, ParserState))
type ConstructParseResult           = (Either (ParserError) (StmtBlockType, ParserState))
type OptionalConstructParseResult   = Maybe (Either (ParserError) ([Stmt], ParserState))
type OptionalStmtParseResult        = Maybe (Either (ParserError) (Stmt, ParserState))

newParserState tokens = ParserState { 
    tokensLeft=tokens, 
    previousToken=Nothing,
    lastIfLabel=0
}

currentToken ParserState { tokensLeft=(TokenWithInfo{token=t}:_) } = t
currentTokenWithInfo ParserState { tokensLeft=(twi:_) } = twi

advanceParser state@ParserState{ tokensLeft=(t:ts) } = state { 
    tokensLeft=ts,
    previousToken=Just(t)
}

advanceIfLabel state@ParserState { lastIfLabel=l } = state {
    lastIfLabel=(l+1)
}

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
matchToken tokens _     = Nothing

matchTokenOrFail :: Token -> ParserState -> (Either ParserError ParserState)
matchTokenOrFail token state = 
    maybeOr
        (Left(ParserErrorExpectedToken((currentTokenWithInfo state), token)))
        (Right . id)
        (matchToken [token] state) 

matchKeywordWithoutAdvance :: String -> ParserState -> Maybe(ParserState)
matchKeywordWithoutAdvance s state@ParserState{ tokensLeft=(TokenWithInfo{token=(TokenIdentifier(is))}:ts) }
    | ((strToLower s) == (strToLower is)) = Just(state)
    | otherwise                           = Nothing
matchKeywordWithoutAdvance _ _ = Nothing

matchKeyword :: String -> ParserState -> Maybe(ParserState)
matchKeyword s state = (matchKeywordWithoutAdvance s state) >>= (Just . advanceParser)

matchKeywordOrFail :: String -> ParserState -> (Either ParserError ParserState)
matchKeywordOrFail keyword state = 
    maybeOr
        (Left(ParserErrorExpectedKeyword((currentTokenWithInfo state), keyword)))
        (Right . id)
        (matchKeyword keyword state)

matchIdentifier :: ParserState -> Maybe(String, ParserState)
matchIdentifier state@ParserState{ tokensLeft=(TokenWithInfo{token=(TokenIdentifier(is))}:ts) } = 
    (Just(is, (advanceParser state)))
matchIdentifier _ = Nothing

matchIdentifierOrFail :: ParserState -> (Either ParserError (String, ParserState))
matchIdentifierOrFail state = 
    maybeOr
        (Left(ParserErrorExpectedIdentifier(currentTokenWithInfo state)))
        (Right . id)
        (matchIdentifier state)

matchInteger :: ParserState -> Maybe(Integer, ParserState)
matchInteger state@ParserState{ tokensLeft=(TokenWithInfo{token=(TokenInteger(i))}:ts) } = 
    (Just(i, (advanceParser state)))
matchInteger _ = Nothing

matchIntegerOrFail :: ParserState -> (Either ParserError (Integer, ParserState))
matchIntegerOrFail state = 
    maybeOr
        (Left(ParserErrorExpectedInteger(currentTokenWithInfo state)))
        (Right . id)
        (matchInteger state)

whileMatchesRight :: [Token] -> ExprParsingFunction -> (Expr, ParserState) -> ExprParseResult
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

convertToBinaryExpr :: Expr -> Maybe(TokenWithInfo) -> ExprParseResult -> ExprParseResult
convertToBinaryExpr _ _ result@(Left(_)) = result
convertToBinaryExpr _ (Nothing) (Right(expr, state)) = (Left(ParserErrorUnknown(currentTokenWithInfo(state))))
convertToBinaryExpr expr (Just(twi@TokenWithInfo{ token=token })) (Right(expr2, state)) =
    maybeOr 
        (Left(ParserErrorUnexpectedToken(twi)))
        (\t -> (Right(ExprBin(expr, t, expr2), state)))
        (tokenToBinaryOp token)

convertToUnaryExpr :: Maybe(TokenWithInfo) -> ExprParseResult -> ExprParseResult
convertToUnaryExpr _ result@(Left(_)) = result
convertToUnaryExpr (Nothing) (Right(expr, state)) = (Left(ParserErrorUnknown(currentTokenWithInfo(state))))
convertToUnaryExpr (Just(twi@TokenWithInfo{ token=token })) (Right(expr, state)) = 
    maybeOr
        (Left(ParserErrorUnexpectedToken(twi))) 
        (\t -> (Right(ExprUn(t, expr), state)))
        (tokenToUnaryOp token)

parseBinaryRightLoop :: ExprParsingFunction -> [Token] -> ExprParsingFunction -> ParserState -> ExprParseResult
parseBinaryRightLoop a operators b state =
    (a state) >>= whileMatchesRight operators b

parseBinaryLeftLoop :: ExprParsingFunction -> [Token] -> ParserState -> ExprParseResult
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

parseUnaryLeftLoop :: ExprParsingFunction -> [Token] -> ParserState -> ExprParseResult
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

-- match an expression list beginning with a comma
exprList :: ParserState -> (Either ParserError ([Expr], ParserState))
exprList orgState =
    maybeOr
        (Right([], orgState))
        (\state -> 
            (expression state) >>=
                \res@(expr, state) ->
                    (exprList state) >>=
                        \(exprs, state) -> 
                            (Right(expr:exprs, state))
        ) 
        (matchToken [TokenComma] orgState)

-- match an integer list beginning with an integer
integerList :: ParserState -> (Either ParserError ([Integer], ParserState))
integerList state = 
    (matchIntegerOrFail state) >>=
    \(integer, state) -> 
        maybeOr
            (Right([integer], state))
            (\state ->
                (integerList state) >>=
                \(intList, state) ->
                    (Right(integer:intList, state))
            )
            (matchToken [TokenComma] state)

advanceResult :: ExprParseResult -> ExprParseResult
advanceResult = (flip (>>=)) (\(expr, state) -> Right(expr, (advanceParser state)))

-- match an integer list beginning with a comma
identifierList :: ParserState -> (Either ParserError ([String], ParserState))
identifierList orgState =
    maybeOr
        (Right([], orgState))
        (\state -> 
            (matchIdentifierOrFail state) >>=
                \res@(str, state) ->
                    (identifierList state) >>=
                        \(strs, state) -> 
                            (Right(str:strs, state))
        ) 
        (matchToken [TokenComma] orgState)

type ExprParsingFunction                = ParserState -> ExprParseResult
type StmtParsingFunction                = ParserState -> StmtParseResult
type ConstructParsingFunction           = ParserState -> ConstructParseResult
type OptionalConstructParsingFunction   = ParserState -> OptionalConstructParseResult
type OptionalStmtParsingFunction        = ParserState -> OptionalStmtParseResult

--    elseIfStmt, elseStmt,
--    loopControl, 

program :: ConstructParsingFunction

executionPart, executableConstruct, 
    ifConstruct, doConstruct, actionStmt :: OptionalConstructParsingFunction

assignmentStmt, gotoStmt, ifStmt, computedGotoStmt,
    writeStmt, readStmt :: OptionalStmtParsingFunction

expression, equivalence, equivOperand, 
    orOperand, 
    andOperand, level4Expr, level2Expr, addOperand, 
    multOperand, level1Expr, 
    primary :: ExprParsingFunction

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
                    }) -> Left(ParserErrorExpectedToken(t, TokenRightParen))
        otherwise -> Left(ParserErrorExpectedExpression(twi))

-- statement parsers

skipSemicolons :: ParserState -> ParserState
skipSemicolons state =
    maybeOr
        state
        (skipSemicolons)
        (matchToken [TokenSemicolon] state)

program state = 
    (matchKeywordOrFail "program" (skipSemicolons state)) >>=
    (matchIdentifierOrFail) >>=
    \(id, state) -> 
        (block state) >>=
        \(stmtBlock, state) ->
            (matchKeywordOrFail "end" (skipSemicolons state)) >>=
            (matchKeywordOrFail "program") >>=
            (\state -> Right(stmtBlock, state))

block :: ParserState -> (Either ParserError (StmtBlockType, ParserState))
block state = 
    maybeOr
        (Left(ParserErrorExpectedBlock(currentTokenWithInfo state)))
        (\result -> result >>= 
            \(stmts, state) -> Right(StmtBlockType(stmts), state))
        (blockInner state)

blockInner :: ParserState -> Maybe(Either ParserError ([Stmt], ParserState))
blockInner state =
    ((matchToken [TokenSemicolon] state) >>= (blockInner)) `altM`
    ((executionPart state) >>=
    \result -> Just $ result >>=
        \(orgStmts, state) -> 
            maybeOr
                (Right(orgStmts, state))
                (\result -> result >>= 
                    \(stmts, state) -> Right(orgStmts ++ stmts, state))
                (blockInner state))

executionPart state = 
    ((matchInteger state) >>=
    \(label, state) -> 
        (executableConstruct state) >>=
        \state -> Just $ 
            state >>= 
            \(stmt:stmts, state) -> 
                Right(StmtLabeled(LabelExplicit, label, stmt):stmts, state)) 
    `altM` 
    (executableConstruct state)

-- if parser and flattener (the most cursed thing in this project)

matchIfPrelude state =
    (matchKeyword "if" state) >>=
    \postIfKeywordState -> Just $
        (matchTokenOrFail TokenLeftParen postIfKeywordState) >>=
        (expression) >>=
        (\(condExpr, state) ->
            (matchTokenOrFail TokenRightParen state) >>=
            (matchKeywordOrFail "then") >>=
            \state -> Right(condExpr, state))

ifConstruct state = 
    (matchIfPrelude state) >>=
    \result -> Just $ result >>=
        (\(condExpr, preIfBlockState) ->
            (block (advanceIfLabel preIfBlockState)) >>=
                \(StmtBlockType(ifStmts), postBlockState) ->
                    case (matchKeyword "else" (skipSemicolons postBlockState)) of
                        Nothing -> 
                            (matchKeywordOrFail "end" (skipSemicolons postBlockState)) >>=
                            (matchKeywordOrFail "if") >>= 
                            \(postIfState) ->
                                (Right(StmtIntCompiledIf(ExprUn(UnOpNot, condExpr), 
                                        (lastIfLabel preIfBlockState)):ifStmts ++ 
                                        [StmtLabeled(LabelIf, (lastIfLabel preIfBlockState), StmtNoop)],
                                    postIfState))
                        Just(postElseState) -> 
                            case (matchKeywordWithoutAdvance "if" $ skipSemicolons postElseState) of
                                Nothing ->  
                                    (block postElseState) >>=
                                    \(StmtBlockType(fstElseStmt:elseStmts), postElseState) ->
                                        (matchKeywordOrFail "end" $ skipSemicolons postElseState) >>=
                                        (matchKeywordOrFail "if") >>= 
                                        \(postIfState) ->
                                            Right(
                                                (StmtIntCompiledIf(ExprUn(UnOpNot, condExpr), (lastIfLabel preIfBlockState)):ifStmts ++ [StmtAbsoluteGoto(LabelIf, (lastIfLabel postIfState)+1)])
                                                ++ 
                                                (StmtLabeled(LabelIf, (lastIfLabel preIfBlockState), fstElseStmt):elseStmts ++ [StmtLabeled(LabelIf, (lastIfLabel postIfState)+1, StmtNoop)]),
                                                (advanceIfLabel postIfState))
                                Just(preNewIfState) -> 
                                    maybeOr
                                        (Left(ParserErrorExpectedIfConstruct((currentTokenWithInfo preNewIfState))))
                                        (\result -> result >>= 
                                            \(fstElseIfStmt:elseIfStmts, postElseIfState) -> 
                                                Right(
                                                    (StmtIntCompiledIf(ExprUn(UnOpNot, condExpr), (lastIfLabel preIfBlockState)):ifStmts ++ [StmtAbsoluteGoto(LabelIf, (lastIfLabel postElseIfState)+1)]
                                                    ++
                                                    StmtLabeled(LabelIf, (lastIfLabel preIfBlockState), fstElseIfStmt):elseIfStmts ++ [StmtLabeled(LabelIf, (lastIfLabel postElseIfState)+1, StmtNoop)]),
                                                    (advanceIfLabel postElseIfState)))
                                        (ifConstruct preNewIfState))

doConstruct state = Nothing
--loopControl = expression
--block = expression

executableConstruct state = 
    (actionStmt state)  `altM`
    (ifConstruct state) `altM`
    (doConstruct state) 

matchGoTo state = 
    ((matchKeyword "go" state) >>= (matchKeyword "to")) `altM`
    (matchKeyword "goto" state)

gotoStmt state = 
    (matchGoTo state) >>=
    \state -> Just $
        (matchIntegerOrFail state) >>=
        \(label, state) ->
            Right(StmtAbsoluteGoto(LabelExplicit, label), state)

-- TODO optional comma after list
computedGotoStmt state = 
    (matchGoTo state) >>=
    (matchToken [TokenLeftParen]) >>=
    \state -> Just $ 
        (integerList state) >>=
        \(list, state) ->
            (matchTokenOrFail TokenRightParen state) >>=
            (expression) >>=
            \(expr, state) ->
                (Right(StmtComputedGoto(list, expr), state))

ifStmt state = 
    (matchKeyword "if" state) >>=
    \state ->
        (matchToken [TokenLeftParen] state) >>=
        (\state -> case (expression state) of
            Right(res) -> Just(res)
            Left(_) -> Nothing) >>=
                \(expr, state) ->
                    (matchToken [TokenRightParen] state) >>= 
                    matchInteger >>=
                    \(a, state) -> Just $
                        (matchTokenOrFail TokenComma state) >>=
                        matchIntegerOrFail >>=
                        \(b, state) ->
                            (matchTokenOrFail TokenComma state) >>=
                            matchIntegerOrFail >>=
                            \(c, state) ->
                                Right(StmtArithmeticIf(expr, a, b, c), state)

assignmentStmt state = 
    (matchIdentifier state) >>=
    \(id, state) -> 
        (matchToken [TokenEq] state) >>=
        \state -> Just $
            (expression state) >>=
            (\(expr, state) -> Right(StmtAssign(id, expr), state))
            

actionStmt state = 
    ((ifStmt state) `altM`
    (computedGotoStmt state) `altM`
    (gotoStmt state) `altM`
    (readStmt state) `altM`
    (writeStmt state) `altM`
    (assignmentStmt state)) >>=
        \result -> Just $ result >>=
            \(stmt, state) -> (Right([stmt], state))

readStmt state = 
    (matchKeyword "read" state) >>= 
    \state -> Just $
        (matchFormat state) >>=
        identifierList >>=
            \(exprs, state) ->
                (Right(StmtRead(exprs), state))

writeStmt state = 
    (matchKeyword "write" state) >>= 
    \state -> Just $
        (matchFormat state) >>=
        exprList >>=
            \(exprs, state) ->
                (Right(StmtWrite(exprs), state))
 
matchFormat state = 
    (matchTokenOrFail TokenLeftParen state)    >>=
    (matchTokenOrFail TokenStar)               >>=
    (matchTokenOrFail TokenComma)              >>=
    (matchTokenOrFail TokenStar)               >>=
    (matchTokenOrFail TokenRightParen)

matchIOBody :: ([Expr] -> Stmt) -> ParserState -> StmtParseResult
matchIOBody constr state = 
    (matchFormat state) >>=
    exprList >>=
        \(exprs, state) ->
            (Right(constr(exprs), state))

