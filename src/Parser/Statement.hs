module Parser.Statement ( 
    program
) where

import Debug.Trace
import Ast.AstTypes
import Ast.TokenTypes
import Utils.Utils
import Parser.Expression
import Parser.Utils
import Parser.State

advanceIfLabel state@ParserState { lastIfLabel=l } = state {
    lastIfLabel=(l+1)
}

advanceDoVarNum state@ParserState { lastDoVarNum=l } = state {
    lastDoVarNum=(l+1)
}

type StmtParseResult                = Either ParserError (Stmt, ParserState)
type ConstructParseResult           = Either ParserError (StmtBlockType, ParserState)
type OptionalConstructParseResult   = Maybe (Either ParserError ([Stmt], ParserState))
type OptionalStmtParseResult        = Maybe (Either ParserError (Stmt, ParserState))

type ConstructParsingFunction           = ParserState -> ConstructParseResult
type OptionalConstructParsingFunction   = ParserState -> OptionalConstructParseResult
type OptionalStmtParsingFunction        = ParserState -> OptionalStmtParseResult

program :: ConstructParsingFunction

executionPart, executableConstruct, 
    ifConstruct, doConstruct, actionStmt :: OptionalConstructParsingFunction

assignmentStmt, gotoStmt, ifStmt, computedGotoStmt,
    writeStmt, readStmt :: OptionalStmtParsingFunction

-- statement parsers

program state = 
    (matchKeywordOrFail "program" (skipSemicolons state)) >>=
    (matchIdentifierOrFail) >>=
    \(_, state) -> 
        (block state) >>=
        \(stmtBlock, state) ->
            (matchKeywordOrFail "end" (skipSemicolons state)) >>=
            (matchKeywordOrFail "program") >>=
            (\state -> Right(stmtBlock, state))

block :: ParserState -> Either ParserError (StmtBlockType, ParserState)
block state = do
    (stmts, state) <- (flattenME
        (ParserErrorExpectedBlock $ currentTokenWithInfo state)
        (blockInner state)) 
    Right(StmtBlockType(stmts), state)

blockInner :: ParserState -> Maybe (Either ParserError ([Stmt], ParserState))
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
                Right $ ((StmtLabeled (NamespaceVisible, label) stmt):stmts, state)) 
    `altM` 
    (executableConstruct state)

-- if parser and flattener 

matchIfPrelude state =
    (matchKeyword "if" state) >>=
    \postIfKeywordState -> Just $
        (matchTokenOrFail TokenLeftParen postIfKeywordState) >>=
        (expression) >>=
        \(condExpr, state) ->
            (matchTokenOrFail TokenRightParen state) >>=
            (matchKeywordOrFail "then") >>=
            \state -> Right(condExpr, state)

matchIfEnd state = 
    (matchKeywordOrFail "end" (skipSemicolons state)) >>= (matchKeywordOrFail "if")

compileSimpleIf condExpr label ifStmts = 
    (StmtIntCompiledIf (ExprUn UnOpNot condExpr) (NamespaceIf, label)):ifStmts ++ 
        [StmtLabeled (NamespaceIf, label) StmtNoop]

compileElseIf condExpr preIfLabel ifStmts postIfLabel (fstElseStmt:elseStmts) = 
    ((StmtIntCompiledIf (ExprUn UnOpNot condExpr) (NamespaceIf, preIfLabel)):ifStmts ++ 
        [StmtAbsoluteGoto (NamespaceIf, postIfLabel)]) ++
    ((StmtLabeled (NamespaceIf, preIfLabel) fstElseStmt):elseStmts ++ 
        [StmtLabeled (NamespaceIf, postIfLabel) StmtNoop])

tryMatchEndGoto state =
    maybeOr
        (Right([], state))
        (\(endLabel, state) -> 
            Right([StmtLabeled (NamespaceVisible, endLabel) StmtNoop], state))
        (matchInteger (skipSemicolons state))

parseIfBody condExpr preIfBlockState = do
    (StmtBlockType(ifStmts), postBlockState) <- block (advanceIfLabel preIfBlockState)
    case (matchKeyword "else" (skipSemicolons postBlockState)) of
        Nothing -> do
            (extraEndGoto, state) <- tryMatchEndGoto postBlockState
            postIfState <- matchIfEnd state
            return $ ((compileSimpleIf 
                        condExpr 
                        (lastIfLabel preIfBlockState) 
                        ifStmts) ++ extraEndGoto,
                        postIfState)
        Just(postElseState) -> 
            let preNewIfState = (skipSemicolons postElseState) in
            case (matchKeyword "if" preNewIfState) of
                Nothing -> do
                    (StmtBlockType(elseStmts), postElseState) <- block postElseState
                    postIfState <- matchIfEnd postElseState
                    return $ (compileElseIf
                                condExpr
                                (lastIfLabel preIfBlockState)
                                ifStmts
                                ((lastIfLabel postIfState)+1)
                                elseStmts,
                                (advanceIfLabel postIfState))
                Just(_) -> 
                    maybeOr
                        (Left $ ParserErrorExpectedIfConstruct $ 
                            (currentTokenWithInfo preNewIfState))
                        (\result -> result >>= 
                            \(fstElseStmts, postElseIfState) -> 
                                Right $ (compileElseIf
                                            condExpr
                                            (lastIfLabel preIfBlockState)
                                            ifStmts 
                                            ((lastIfLabel postElseIfState)+1)
                                            fstElseStmts,
                                        (advanceIfLabel postElseIfState)))
                        (ifConstruct preNewIfState)

ifConstruct state = do
    result <- matchIfPrelude state
    return $ result >>= 
        \(expr, state) -> parseIfBody expr state

-- do parser and flattener

tryMatchIncrement postLimitState =
    case (matchToken [TokenComma] postLimitState) of
        Nothing ->
            (Right((ExprInteger(1)), postLimitState))
        Just(postLimitCommaState) ->
            (expression postLimitCommaState) >>=
            \(incrementExpr, postIncrementState) ->
                (Right(incrementExpr, postIncrementState))

parseDoBody assignment@(StmtAssign var _) postAssignmentState = do
    state <- matchTokenOrFail TokenComma postAssignmentState
    (limitExpr, postLimitState) <- expression state
    (incExpr, postIncrementState) <- tryMatchIncrement postLimitState
    let doNum           = (lastDoVarNum state) 
        doEndNum        = (lastDoVarNum state) + 1
        doLimit         = (NamespaceDo, "l" ++ (show doNum))
        doInc           = (NamespaceDo, "i" ++ (show doNum)) 
        postLblState    = (advanceDoVarNum (advanceDoVarNum postIncrementState))
    (block postLblState) >>=
        \(StmtBlockType(fstDoBlock:doBlock), postBlockState) -> do
            (extraEndGoto, state) <- tryMatchEndGoto postBlockState
            state <- matchKeywordOrFail "end" (skipSemicolons state)
            postDoStatementState <- matchKeywordOrFail "do" state
            return $ (
                assignment:
                (StmtAssign doLimit limitExpr):
                (StmtAssign doInc incExpr):
                (StmtLabeled (NamespaceDo, doNum) fstDoBlock):
                doBlock ++
                (StmtIntCompiledIf
                    (ExprBin
                        (ExprIdentifier var)
                        BinOpGeq
                        (ExprIdentifier doLimit))
                    (NamespaceDo, doEndNum)):
                (StmtAssign
                    var
                    (ExprBin
                        (ExprIdentifier var)
                        BinOpAdd
                        (ExprIdentifier doInc))):
                (StmtAbsoluteGoto (NamespaceDo, doNum)):
                (StmtLabeled (NamespaceDo, doEndNum) StmtNoop):extraEndGoto,
                postDoStatementState)

parseDoBody _ postAssignmentState =
    Left $ ParserErrorUnknown $ currentTokenWithInfo postAssignmentState

doConstruct state = do
    state <- (matchKeyword "do" state)
    let crttoken      = currentTokenWithInfo state
        assignmentErr = Just $ Left $ ParserErrorExpectedAssignment crttoken 
    result <- (assignmentStmt state) `altM` assignmentErr
    return $ result >>=
        \(assignment, postAssignmentState) -> parseDoBody assignment postAssignmentState

-- other contructs and statements

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
            Right(StmtAbsoluteGoto (NamespaceVisible, label), state)

computedGotoStmt state = 
    (matchGoTo state) >>=
    (matchToken [TokenLeftParen]) >>=
    \state -> Just $ 
        (matchList matchIntegerOrFail state) >>=
        \(list, state) ->
            (matchTokenOrFail TokenRightParen state) >>=
            (expression) >>=
            \(expr, state) ->
                Right $ (StmtComputedGoto list expr, state)

ifStmt state = do
    state <- (matchKeyword "if" state)
    state <- (matchToken [TokenLeftParen] state)
    (expr, state) <- nothingOnLeft (expression state)
    state <- (matchToken [TokenRightParen] state)
    (a, state) <- (matchInteger state)
    return $ do
        state <- (matchTokenOrFail TokenComma state) 
        (b, state) <- (matchIntegerOrFail state)
        state <- (matchTokenOrFail TokenComma state)
        (c, state) <- (matchIntegerOrFail state)
        return $ ((StmtArithmeticIf expr a b c), state)

assignmentStmt state = 
    (matchIdentifier state) >>=
    \(id, state) -> 
        (matchToken [TokenEq] state) >>=
        \state -> Just $
            (expression state) >>=
            (\(expr, state) -> Right $ ((StmtAssign (NamespaceVisible, id) expr), state))
            

actionStmt state = 
    ((ifStmt state) `altM`
    (computedGotoStmt state) `altM`
    (gotoStmt state) `altM`
    (readStmt state) `altM`
    (writeStmt state) `altM`
    (assignmentStmt state)) >>=
        \result -> Just $ result >>=
            \(stmt, state) -> (Right([stmt], state))

readStmt = matchIOStmt "read" StmtRead matchIdentifierOrFail

writeStmt = matchIOStmt "write" StmtWrite expression

matchIOStmt :: String -> ([a] -> Stmt) -> 
               (ParserState -> (Either ParserError (a, ParserState))) -> 
               ParserState -> OptionalStmtParseResult
matchIOStmt keyword constr listParser state = 
    (matchKeyword keyword state) >>= 
    \state -> Just $
        (matchFormat state) >>=
        \postFormatState ->
            maybeOr
                (Right(constr([]), postFormatState))
                (\postCommaState ->
                    (matchList listParser postCommaState) >>=
                    \(exprList, postExprListState) ->
                        (Right(constr(exprList), postExprListState)))
                (matchToken [TokenComma] postFormatState)
 
matchFormat state = 
    (matchTokenOrFail TokenLeftParen state)    >>=
    (matchTokenOrFail TokenStar)               >>=
    (matchTokenOrFail TokenComma)              >>=
    (matchTokenOrFail TokenStar)               >>=
    (matchTokenOrFail TokenRightParen)

