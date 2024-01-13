module Eval (
    eval,
    evalContext
) where

import System.IO
import AstTypes
import Map
import Utils
import GotoMap

data Value = 
    ValueInteger(Integer) | 
    ValueString(String) | 
    ValueFloat(Double) | 
    ValueBool(Bool)
    deriving(Show)

data EvalError = 
    EvalErrorUnknownVariable(String) | 
    EvalErrorUnmatchedTypes | 
    EvalErrorInvalidOp | 
    EvalErrorNotImplemented
    deriving(Show)

type VariableMap = (SimpleMap String Value)

data EvalContext = EvalContext { 
    variableMap :: VariableMap,
    gotoMap :: GotoMap
}
    deriving(Show)

evalContext = EvalContext { 
    variableMap=simpleMap,
    gotoMap=simpleMap
}

type EvalResult = (Either EvalError Value)

--execBlock :: EvalContext -> [Stmt] -> IO (Either EvalError EvalContext)
--execBlock context stmts = do

execStmt :: EvalContext -> Stmt -> IO (Either EvalError EvalContext)

execStmt context (StmtLabeled(_, _, stmt)) = execStmt context stmt

execStmt context (StmtAssign(name, expr)) = do
    return $ (eval context expr) >>= \res ->
        (return (evalContext { variableMap=(mapInsert name res (variableMap context)) }))

execStmt context (StmtRead([])) = return $ return $ context

--execStmt context (StmtRead(ids)) = do
--    line <- readLine
--
--    (putStr $ show (eval context expr)) >>
--    (execStmt context (StmtRead(ids)))

execStmt context (StmtWrite(expr:exprs)) = 
    (putStr $ show (eval context expr)) >>
    (execStmt context (StmtWrite(exprs)))

execStmt context (StmtWrite([])) = do
    putStrLn ""
    return (return context)

execStmt context (StmtWrite(expr:exprs)) = 
    (putStr $ show (eval context expr)) >>
    (execStmt context (StmtWrite(exprs)))

-- expression evaluation

eval :: EvalContext -> Expr -> EvalResult

eval ctx (ExprBin(a, op, b))    = handleBinaryEvalResults (eval ctx a) op (eval ctx b)
eval ctx (ExprUn(op, a))        = handleUnaryEvalResult op (eval ctx a)
eval _ (ExprString(val))      = (Right(ValueString(val)))
eval _ (ExprInteger(val))     = (Right(ValueInteger(val)))
eval _ (ExprFloat(val))       = (Right(ValueFloat(val)))
eval _ (ExprBool(val))        = (Right(ValueBool(val)))
eval ctx (ExprIdentifier(str)) = 
    case (mapLookup str (variableMap ctx)) of
        Just(val) -> (Right(val))
        Nothing -> (Left(EvalErrorUnknownVariable(str)))

handleBinaryEvalResults :: EvalResult -> BinaryOp -> EvalResult -> EvalResult
handleBinaryEvalResults (Right a) op (Right b)         = evalBinary a op b 
handleBinaryEvalResults (Left err) _ _                 = (Left err)
handleBinaryEvalResults _ _ (Left err)                 = (Left err)

evalBinary :: Value -> BinaryOp -> Value -> EvalResult
evalBinary (ValueInteger a) BinOpAdd  (ValueInteger b) = (Right(ValueInteger (a+b)))
evalBinary (ValueInteger a) BinOpSub  (ValueInteger b) = (Right(ValueInteger (a-b)))
evalBinary (ValueInteger a) BinOpMult (ValueInteger b) = (Right(ValueInteger (a*b)))
evalBinary (ValueInteger a) BinOpDiv  (ValueInteger b) = (Right(ValueInteger (a`div`b)))
evalBinary (ValueInteger a) BinOpPow  (ValueInteger b) = (Right(ValueInteger (a^b)))

evalBinary (ValueInteger a) BinOpEq   (ValueInteger b) = (Right(ValueBool (a == b)))
evalBinary (ValueInteger a) BinOpNeq  (ValueInteger b) = (Right(ValueBool (a /= b)))
evalBinary (ValueInteger a) BinOpGt   (ValueInteger b) = (Right(ValueBool (a > b)))
evalBinary (ValueInteger a) BinOpGeq  (ValueInteger b) = (Right(ValueBool (a >= b)))
evalBinary (ValueInteger a) BinOpLt   (ValueInteger b) = (Right(ValueBool (a < b)))
evalBinary (ValueInteger a) BinOpLeq  (ValueInteger b) = (Right(ValueBool (a <= b)))

evalBinary (ValueFloat a)   BinOpAdd  (ValueFloat b)   = (Right(ValueFloat (a+b)))
evalBinary (ValueFloat a)   BinOpSub  (ValueFloat b)   = (Right(ValueFloat (a-b)))
evalBinary (ValueFloat a)   BinOpMult (ValueFloat b)   = (Right(ValueFloat (a*b)))
evalBinary (ValueFloat a)   BinOpDiv  (ValueFloat b)   = (Right(ValueFloat (a/b)))
evalBinary (ValueFloat a)   BinOpPow  (ValueFloat b)   = (Right(ValueFloat (a**b)))

evalBinary (ValueFloat a) BinOpEq   (ValueFloat b) = (Right(ValueBool (a == b)))
evalBinary (ValueFloat a) BinOpNeq  (ValueFloat b) = (Right(ValueBool (a /= b)))
evalBinary (ValueFloat a) BinOpGt   (ValueFloat b) = (Right(ValueBool (a > b)))
evalBinary (ValueFloat a) BinOpGeq  (ValueFloat b) = (Right(ValueBool (a >= b)))
evalBinary (ValueFloat a) BinOpLt   (ValueFloat b) = (Right(ValueBool (a < b)))
evalBinary (ValueFloat a) BinOpLeq  (ValueFloat b) = (Right(ValueBool (a <= b)))

evalBinary (ValueInteger(a)) op (ValueFloat(b))    = evalBinary (ValueFloat $ fromIntegral(a)) op (ValueFloat b)
evalBinary (ValueFloat(a))   op (ValueInteger(b))  = evalBinary (ValueFloat a) op (ValueFloat $ fromIntegral(b))

evalBinary (ValueBool a) BinOpAnd  (ValueBool b) = (Right(ValueBool (a && b)))
evalBinary (ValueBool a) BinOpOr   (ValueBool b) = (Right(ValueBool (a || b)))
evalBinary (ValueBool a) BinOpEq   (ValueBool b) = (Right(ValueBool (a == b)))
evalBinary (ValueBool a) BinOpNeq  (ValueBool b) = (Right(ValueBool (a /= b)))


handleUnaryEvalResult :: UnaryOp -> EvalResult -> EvalResult
handleUnaryEvalResult op (Right a) = evalUnary op a
handleUnaryEvalResult _ (Left err) = (Left err)

evalUnary UnOpNot   (ValueBool a)       = (Right $ ValueBool    $ not    $ a)
evalUnary UnOpMinus (ValueInteger a)    = (Right $ ValueInteger $ negate $ a)
evalUnary UnOpPlus  (ValueInteger a)    = (Right $ ValueInteger $ negate $ a)
evalUnary UnOpMinus (ValueFloat a)      = (Right $ ValueFloat   $ negate $ a)
evalUnary UnOpPlus  (ValueFloat a)      = (Right $ ValueFloat   $ negate $ a)

evalUnary _ _           = (Left EvalErrorInvalidOp)
