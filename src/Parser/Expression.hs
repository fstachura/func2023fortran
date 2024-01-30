module Parser.Expression (
    expression
) where

import Ast.TokenTypes
import Ast.AstTypes
import Utils.Utils
import Parser.Utils
import Parser.State

type ExprParsingFunction                = ParserState -> ExprParseResult
type ExprParseResult                    = Either ParserError (Expr, ParserState)

tokenToBinaryOp :: Token -> Maybe(BinaryOp)
tokenToBinaryOp TokenEqEq   = Just $ BinOpEq
tokenToBinaryOp TokenEq     = Just $ BinOpEq
tokenToBinaryOp TokenNeq    = Just $ BinOpNeq
tokenToBinaryOp TokenEqv    = Just $ BinOpEq
tokenToBinaryOp TokenNeqv   = Just $ BinOpNeq
tokenToBinaryOp TokenGt     = Just $ BinOpGt
tokenToBinaryOp TokenGeq    = Just $ BinOpGeq
tokenToBinaryOp TokenLt     = Just $ BinOpLt
tokenToBinaryOp TokenLeq    = Just $ BinOpLeq
tokenToBinaryOp TokenPlus   = Just $ BinOpAdd
tokenToBinaryOp TokenMinus  = Just $ BinOpSub
tokenToBinaryOp TokenSlash  = Just $ BinOpDiv
tokenToBinaryOp TokenStar   = Just $ BinOpMult
tokenToBinaryOp TokenPow    = Just $ BinOpPow
tokenToBinaryOp TokenAnd    = Just $ BinOpAnd
tokenToBinaryOp TokenOr     = Just $ BinOpOr
tokenToBinaryOp _           = Nothing

tokenToUnaryOp :: Token -> Maybe(UnaryOp)
tokenToUnaryOp TokenNot     = Just $ UnOpNot
tokenToUnaryOp TokenMinus   = Just $ UnOpMinus
tokenToUnaryOp TokenPlus    = Just $ UnOpPlus
tokenToUnaryOp _            = Nothing

convertToBinaryExpr :: Expr -> Maybe(TokenWithInfo) -> ExprParseResult -> ExprParseResult
convertToBinaryExpr _ _ result@(Left(_)) = result
convertToBinaryExpr _ (Nothing) (Right(_, state)) = Left $ ParserErrorUnknown $ currentTokenWithInfo state
convertToBinaryExpr expr (Just(twi@TokenWithInfo{ token=token })) (Right(expr2, state)) =
    maybeOr 
        (Left $ ParserErrorUnexpectedToken $ twi)
        (\t -> Right $ (ExprBin expr t expr2, state))
        (tokenToBinaryOp token)

convertToUnaryExpr :: Maybe(TokenWithInfo) -> ExprParseResult -> ExprParseResult
convertToUnaryExpr _ result@(Left(_)) = result
convertToUnaryExpr (Nothing) (Right(_, state)) = Left $ ParserErrorUnknown $ currentTokenWithInfo state
convertToUnaryExpr (Just(twi@TokenWithInfo{ token=token })) (Right(expr, state)) = 
    maybeOr
        (Left $ ParserErrorUnexpectedToken twi)
        (\t -> Right (ExprUn t expr, state))
        (tokenToUnaryOp token)

whileMatchesRight :: [Token] -> ExprParsingFunction -> (Expr, ParserState) -> ExprParseResult
whileMatchesRight tokens f orgResult@(orgExpr, orgState) =
    maybeOr
        (Right(orgResult))
        (\stateAfterMatch -> 
            (f stateAfterMatch) >>=
                \(newExpr, newState) -> 
                    (convertToBinaryExpr orgExpr (previousToken stateAfterMatch) (Right(newExpr, newState))) >>=
                        \(newExpr, newState) ->
                            (whileMatchesRight tokens f (newExpr, newState)))
        (matchToken tokens orgState)

parseBinaryRightLoop :: ExprParsingFunction -> [Token] -> ExprParsingFunction -> ParserState -> ExprParseResult
parseBinaryRightLoop a operators b state =
    (a state) >>= whileMatchesRight operators b

parseUnary :: [Token] -> ExprParsingFunction -> ParserState -> ExprParseResult
parseUnary tokens f orgState =
    maybeOr
        res
        (\state -> 
            convertToUnaryExpr 
                (previousToken state)
                (f state))
        (matchToken tokens orgState)
    where res = (f orgState)

-- parsers

expression, 
    equivalence, equivOperand, 
    orOperand, andOperand, 
    level4Expr, level2Expr, 
    addOperand, multOperand, 
    primary :: ExprParsingFunction

expression      = equivalence
equivalence     = parseBinaryRightLoop  equivOperand equivOperators equivOperand
equivOperand    = parseBinaryRightLoop  orOperand [TokenOr] orOperand
orOperand       = parseBinaryRightLoop  andOperand [TokenAnd] andOperand
andOperand      = parseUnary            [TokenNot] level4Expr
level4Expr      = parseBinaryRightLoop  level2Expr relOperators level2Expr 
level2Expr      = parseUnary            signOperators (parseBinaryRightLoop addOperand addOperators addOperand)
addOperand      = parseBinaryRightLoop  multOperand multOperators multOperand
multOperand     = parseBinaryRightLoop  primary [TokenPow] multOperand

functionCall state = 
    (matchIdentifierOrFail state) >>=
    \(id, postIdState) ->
        (matchTokenOrFail TokenLeftParen postIdState) >>=
        (matchList expression) >>=
        \(exprList, postExprListState) ->
            (matchTokenOrFail TokenRightParen postExprListState) >>=
            \postRightParenState ->
                Right (ExprCall id exprList, postRightParenState)

primary state@ParserState { tokensLeft=(twi@TokenWithInfo{token=t}:_) } = 
    case t of
        TokenString(s) -> Right (ExprString s, advanceParser state) 
        TokenInteger(i) -> Right (ExprInteger i, advanceParser state)
        TokenFloat(f) -> Right (ExprFloat f, advanceParser state)
        TokenBool(b) -> Right (ExprBool b, advanceParser state)
        TokenIdentifier(i) -> 
            maybeOr
                (Right(ExprIdentifier (NamespaceVisible, i), advanceParser state))
                (\_ -> functionCall state)
                (matchToken [TokenLeftParen] (advanceParser state))
        TokenLeftParen -> (expression $ advanceParser state) >>=
            \x -> case x of
                (expr,
                    state@ParserState{ 
                        tokensLeft=(TokenWithInfo{ token=TokenRightParen }:_) 
                    }) -> Right(expr, advanceParser state)
                (_,
                    ParserState{
                        tokensLeft=(t:_) 
                    }) -> Left(ParserErrorExpectedToken t TokenRightParen)
        _ -> Left(ParserErrorExpectedExpression twi)

