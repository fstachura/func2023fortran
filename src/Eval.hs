module Eval (
    eval,
    execBlock,
    evalContext
) where

import System.IO
import Debug.Trace
import AstTypes
import Map
import Utils
import GotoMap

data Value = 
    ValueInteger(Integer) | 
    ValueString(String) | 
    ValueFloat(Double) | 
    ValueBool(Bool)

instance Show Value where
    show (ValueInteger(i))   = show i
    show (ValueString(s))    = s
    show (ValueFloat(f))     = show f
    show (ValueBool(b))      = show b

data EvalError = 
    EvalErrorUnknownVariable(String) | 
    EvalErrorIncompatibleTypes | 
    EvalErrorInvalidOp | 
    EvalErrorLabelNotFound(Integer) | 
    EvalErrorNotImplemented
    deriving(Show)

type VariableMap = (SimpleMap String Value)

data EvalContext = EvalContext { 
    variableMap :: VariableMap,
    gotoMap :: GotoMap
}
    deriving(Show)

evalContext gotoMap = EvalContext { 
    variableMap=simpleMap,
    gotoMap=gotoMap
}

type EvalResult = (Either EvalError Value)

--execBlock :: EvalContext -> [Stmt] -> IO (Either EvalError EvalContext)
--execBlock context stmts = do

goto :: LabelType -> Integer -> EvalContext -> IO (Either EvalError EvalContext)
goto t l context = do
    case (mapLookup (t, l) (gotoMap context)) of
        Just(res) -> execBlock context res
        Nothing -> return (Left(EvalErrorLabelNotFound(l)))

execBlock :: EvalContext -> [Stmt] -> IO (Either EvalError EvalContext)

execBlock context ((StmtIntCompiledIf(expr, label)):stmts) = do
    case (eval context expr) of
        Right(ifResult) -> 
            if (truthy ifResult) then 
                goto LabelIf label context
            else
                execBlock context stmts
        Left(err) -> return $ Left(err)

execBlock context ((StmtAbsoluteGoto(lt, label)):stmts) = do
    case (mapLookup (lt, label) (gotoMap context)) of
        Just(res) -> execBlock context res
        Nothing -> return (Left(EvalErrorLabelNotFound(label)))

execBlock context ((StmtArithmeticIf(expr, a, b, c)):stmts) = do
    case (eval context expr) of
        Right(ifResult) -> 
            case (castToInt ifResult) of
                Just(val) ->
                    if val < 0 then     
                        goto LabelExplicit a context
                    else if val == 0 then
                        goto LabelExplicit b context
                    else 
                        goto LabelExplicit c context
                Nothing -> return $ Left(EvalErrorIncompatibleTypes)
        Left(err) -> return $ Left(err)

execBlock context ((StmtComputedGoto(labels, expr)):stmts) = do
    case (eval context expr) of
        Right(exprResult) -> 
            case (castToInt exprResult) of
                Just(val) ->
                    if (fromIntegral (val-1)) >= (length labels) || (fromIntegral (val-1)) <= 0 then
                        execBlock context stmts
                    else 
                        goto LabelExplicit (labels !! (fromIntegral (val-1))) context
                Nothing -> return $ Left(EvalErrorIncompatibleTypes)
        Left(err) -> return $ Left(err)

execBlock context ((StmtLabeled(_, _, stmt)):stmts) = execBlock context (stmt:stmts)

execBlock context ((StmtAssign(name, expr)):stmts) = do
    case (eval context expr) of
        Right(res) ->
            (execBlock (context { variableMap=(mapInsert name res (variableMap context)) }) stmts)
        Left(err) -> return $ Left $ err

execBlock context (StmtRead(_):stmts) = execBlock context stmts

execBlock context (StmtWrite(exprs):stmts) = do
    execWrite context exprs
    execBlock context stmts

execBlock context (StmtNoop:stmts) = execBlock context stmts

execBlock context [] = return $ Right $ context

--execStmt context (StmtRead(ids)) = do
--    line <- readLine
--
--    (putStr $ show (eval context expr)) >>
--    (execStmt context (StmtRead(ids)))

execWrite :: EvalContext -> [Expr] -> IO (Either EvalError EvalContext)

execWrite context (expr:exprs) = 
    case (eval context expr) of
        Right(res) -> (putStr $ show $ res) >> (execWrite context exprs)
        Left(err) -> return $ Left $ err

execWrite context [] = do
    putStrLn ""
    return (return context)

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

truthy :: Value -> Bool
truthy (ValueBool a) = a
truthy (ValueInteger a) = a /= 0
truthy (ValueFloat a) = a /= 0
truthy (ValueString a) = (length a) /= 0

castToInt :: Value -> Maybe(Integer)
castToInt (ValueBool a) = Just $ if a then 1 else 0
castToInt (ValueInteger a) = Just $ a
castToInt (ValueFloat a) = Just $ floor a
castToInt (ValueString a) = Nothing

