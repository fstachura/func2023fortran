module Eval.Eval (
    eval,
    execBlock,
    evalContext
) where

import Debug.Trace
import Ast.AstTypes
import Utils.Map
import Utils.Utils
import Eval.BuiltinFunctions
import Eval.EvalTypes

evalContext gotoMap = EvalContext { 
    variableMap=simpleMap,
    gotoMap=gotoMap,
    functionMap=defaultFunctionMap
}

-- utils

skipSpaces (' ':rest) = rest
skipSpaces rest = rest

validateSign :: String -> Bool
validateSign "" = True
validateSign (s:str) = 
    let n = (count ((flip elem) "+-") (s:str)) in
    if n == 0 then True
    else if (n == 1 && (s `elem` "+-")) then True
    else False

validateInt :: String -> Bool
validateInt str = 
    (validateSign str) &&
    (foldl (\acc el -> acc && elem el (digits ++ "+-")) True str)

validateFloat :: String -> Bool
validateFloat str = 
    (validateSign str) &&
    ((count (== '.') str) <= 1) &&
    (foldl (\acc el -> acc && elem el (digits ++ ".+-")) True str)

readVariables :: [String] -> String -> (Either EvalError ([(String, Value)], [String]))
readVariables (var:vars) str =
    let (cur, rest) = (break (== ' ') (skipSpaces str))
        firstChar = (strToLower (take 1 var))
        skipped = (skipSpaces cur)
    in
    if (length cur) == 0 then
        Right ([], var:vars)
    else 
        (readVariables vars rest) >>= 
        \(vals, leftVars) ->
            if (firstChar `elem` ["i", "j", "k", "l", "m"] && validateInt skipped) then
                Right $ ((var, ValueInteger $ read skipped):vals, leftVars)
            else if validateFloat skipped then
                Right $ ((var, ValueFloat $ read skipped):vals, leftVars)
            else
                Left $ EvalErrorReading $ cur

readVariables [] _ = Right $ ([], [])

applyVariables :: [(String, Value)] -> VariableMap -> VariableMap
applyVariables = flip (foldl (\acc (k,v) -> mapInsert (NamespaceVisible, k) v acc))

-- statement executors

goto :: Namespace -> Int -> EvalContext -> IO (Either EvalError EvalContext)
goto t l context = do
    case (mapLookup (t, l) (gotoMap context)) of
        Just(res) -> execBlock context res
        Nothing -> return $ Left $ EvalErrorLabelNotFound l

execBlock :: EvalContext -> [Stmt] -> IO (Either EvalError EvalContext)

execBlock context ((StmtIntCompiledIf expr (namespace, label)):stmts) = do
    case (eval context expr) of
        Right(ifResult) -> 
            if (truthy ifResult) then 
                goto namespace label context
            else
                execBlock context stmts
        Left(err) -> return $ Left err

execBlock context ((StmtAbsoluteGoto(lt, label)):_) = do
    case (mapLookup (lt, label) (gotoMap context)) of
        Just(res) -> execBlock context res
        Nothing -> return $ Left $ EvalErrorLabelNotFound label

execBlock context ((StmtArithmeticIf expr a b c):_) = do
    case (eval context expr) of
        Right(ifResult) -> 
            case (castToFloat ifResult) of
                Just(val) ->
                    let label = if val < 0 then a else if val == 0 then b else c in
                    goto NamespaceVisible label context
                Nothing -> return $ Left EvalErrorIncompatibleTypes
        Left(err) -> return $ Left err

execBlock context ((StmtComputedGoto labels expr):stmts) = do
    case (eval context expr) of
        Right(exprResult) -> 
            case (castToInt exprResult) of
                Just(val) ->
                    if (fromIntegral (val-1)) >= (length labels) || (fromIntegral (val-1)) <= 0 then
                        execBlock context stmts
                    else 
                        goto NamespaceVisible (labels !! (fromIntegral (val-1))) context
                Nothing -> return $ Left $ EvalErrorIncompatibleTypes
        Left(err) -> return $ Left $ err

execBlock context ((StmtLabeled _ stmt):stmts) = execBlock context (stmt:stmts)

execBlock context ((StmtAssign name expr):stmts) = do
    case (eval context expr) of
        Right(res) ->
            (execBlock (context { variableMap=(mapInsert name res $ variableMap context) }) stmts)
        Left(err) -> return $ Left $ err

execBlock context (StmtRead(vars):stmts) = do
    result <- execRead context vars 
    case result of 
        Right(context) -> execBlock context stmts
        Left(err) -> return $ Left(err)

execBlock context (StmtWrite(exprs):stmts) = do
    (execWrite context exprs) >>=
        \val -> case val of
            Right(_) -> execBlock context stmts
            Left(err) -> return $ Left(err)

execBlock context (StmtNoop:stmts) = execBlock context stmts

execBlock context [] = return $ Right $ context

execRead :: EvalContext -> [String] -> IO (Either EvalError EvalContext)
execRead context vars = do
    line <- getLine
    case readVariables vars line of
        Right(values, left) -> 
            let newContext = context{ variableMap=(applyVariables values $ variableMap context) } in
            if (length left) == 0 then
                return $ Right $ newContext 
            else
                execRead newContext left
        Left(err) -> return $ Left $ err

execWrite :: EvalContext -> [Expr] -> IO (Either EvalError EvalContext)

execWrite context (expr:exprs) = 
    case (eval context expr) of
        Right(res) -> do
            putStr $ show $ res
            putStr " "
            execWrite context exprs
        Left(err) -> return $ Left $ err

execWrite context [] = do
    putStrLn ""
    return (return context)

-- expression evaluation

eval :: EvalContext -> Expr -> EvalResult

eval ctx (ExprBin a op b)    = handleBinaryEvalResults (eval ctx a) op (eval ctx b)
eval ctx (ExprUn op a)       = handleUnaryEvalResult op (eval ctx a)
eval _ (ExprString val)      = Right $ ValueString val
eval _ (ExprInteger val)     = Right $ ValueInteger val
eval _ (ExprFloat val)       = Right $ ValueFloat val
eval _ (ExprBool val)        = Right $ ValueBool val
eval ctx (ExprIdentifier var) = 
    case (mapLookup var (variableMap ctx)) of
        Just(val) -> Right val
        Nothing -> Left $ (uncurry EvalErrorUnknownVariable) $ var
eval ctx (ExprCall fun args) =
    case (mapLookup fun (functionMap ctx))  of
        Just(f) -> (mapM (eval ctx) args) >>= 
                   \evalArgs -> f evalArgs ctx
        Nothing -> Left $ EvalErrorUnknownFunction $ fun

handleBinaryEvalResults :: EvalResult -> BinaryOp -> EvalResult -> EvalResult
handleBinaryEvalResults (Right a) op (Right b)         = evalBinary a op b 
handleBinaryEvalResults (Left err) _ _                 = Left err
handleBinaryEvalResults _ _ (Left err)                 = Left err

evalBinary :: Value -> BinaryOp -> Value -> EvalResult

evalBinary (ValueInteger a) op (ValueInteger b) = 
    case op of 
        BinOpAdd    -> return $ ValueInteger $ a + b
        BinOpSub    -> return $ ValueInteger $ a - b
        BinOpMult   -> return $ ValueInteger $ a * b
        BinOpDiv    -> return $ ValueInteger $ a `div` b
        BinOpPow    -> return $ ValueInteger $ a ^ b
        BinOpEq     -> return $ ValueBool    $ a == b
        BinOpNeq    -> return $ ValueBool    $ a /= b
        BinOpGt     -> return $ ValueBool    $ a >  b
        BinOpGeq    -> return $ ValueBool    $ a >= b
        BinOpLt     -> return $ ValueBool    $ a <  b
        BinOpLeq    -> return $ ValueBool    $ a <= b
        _           -> Left EvalErrorInvalidOp

evalBinary (ValueFloat a) op (ValueFloat b) = 
    case op of 
        BinOpAdd    -> return $ ValueFloat   $ a + b
        BinOpSub    -> return $ ValueFloat   $ a - b
        BinOpMult   -> return $ ValueFloat   $ a * b
        BinOpDiv    -> return $ ValueFloat   $ a / b
        BinOpPow    -> return $ ValueFloat   $ a ** b
        BinOpEq     -> return $ ValueBool    $ a == b
        BinOpNeq    -> return $ ValueBool    $ a /= b
        BinOpGt     -> return $ ValueBool    $ a >  b
        BinOpGeq    -> return $ ValueBool    $ a >= b
        BinOpLt     -> return $ ValueBool    $ a <  b
        BinOpLeq    -> return $ ValueBool    $ a <= b
        _           -> Left EvalErrorInvalidOp

evalBinary (ValueInteger(a)) op (ValueFloat(b))    = evalBinary (ValueFloat $ fromIntegral a) op (ValueFloat b)
evalBinary (ValueFloat(a))   op (ValueInteger(b))  = evalBinary (ValueFloat a) op (ValueFloat $ fromIntegral b)

evalBinary (ValueBool a) op (ValueBool b) = 
    case op of
        BinOpAnd    -> return $ ValueBool $ a && b
        BinOpOr     -> return $ ValueBool $ a || b
        BinOpEq     -> return $ ValueBool $ a == b
        BinOpNeq    -> return $ ValueBool $ a /= b
        _           -> Left EvalErrorInvalidOp

evalBinary _ _ _ = Left EvalErrorInvalidOp

handleUnaryEvalResult :: UnaryOp -> EvalResult -> EvalResult
handleUnaryEvalResult op (Right a) = evalUnary op a
handleUnaryEvalResult _ (Left err) = Left err

evalUnary UnOpNot   (ValueBool a)       = return $ ValueBool    $ not    $ a
evalUnary UnOpMinus (ValueInteger a)    = return $ ValueInteger $ negate $ a
evalUnary UnOpPlus  (ValueInteger a)    = return $ ValueInteger $ a
evalUnary UnOpMinus (ValueFloat a)      = return $ ValueFloat   $ negate $ a
evalUnary UnOpPlus  (ValueFloat a)      = return $ ValueFloat   $ a

evalUnary _ _           = Left EvalErrorInvalidOp

-- eval conversion utils

truthy :: Value -> Bool
truthy (ValueBool a)    = a
truthy (ValueInteger a) = a /= 0
truthy (ValueFloat a)   = a /= 0
truthy (ValueString a)  = (length a) /= 0

castToInt :: Value -> Maybe Int
castToInt (ValueBool a)     = Just $ if a then 1 else 0
castToInt (ValueInteger a)  = Just $ a
castToInt (ValueFloat a)    = Just $ floor a
castToInt (ValueString _)   = Nothing

castToFloat :: Value -> Maybe Double
castToFloat (ValueBool a)       = Just $ if a then 1 else 0
castToFloat (ValueInteger a)    = Just $ fromIntegral $ a
castToFloat (ValueFloat a)      = Just $ a
castToFloat (ValueString _)     = Nothing

