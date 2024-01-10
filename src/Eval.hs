module Eval (
    eval,
    defaultEvalContext
) where

import AstTypes

data Value = ValueInteger(Integer) | ValueString(String) | ValueFloat(Double) | ValueBool(Bool)
    deriving(Show)

data EvalError = EvalErrorUnknownVariable | EvalErrorUnmatchedTypes | EvalErrorInvalidOp | EvalErrorNotImplemented
    deriving(Show)

data EvalContext = EvalContext { }
    deriving(Show)

defaultEvalContext = EvalContext {  }

type EvalResult = (Either Value EvalError)

eval :: EvalContext -> Expr -> EvalResult

eval ctx (ExprBin(a, op, b))    = handleBinaryEvalResults (eval ctx a) op (eval ctx b)
eval ctx (ExprUn(op, a))        = handleUnaryEvalResult op (eval ctx a)
eval ctx (ExprString(val))      = (Left(ValueString(val)))
eval ctx (ExprInteger(val))     = (Left(ValueInteger(val)))
eval ctx (ExprFloat(val))       = (Left(ValueFloat(val)))
eval ctx (ExprBool(val))        = (Left(ValueBool(val)))
-- eval _ ExprIdentifier(String) = 


handleBinaryEvalResults :: EvalResult -> BinaryOp -> EvalResult -> EvalResult
handleBinaryEvalResults (Left a) op (Left b)            = evalBinary a op b 
handleBinaryEvalResults (Right err) _ _                 = (Right err)
handleBinaryEvalResults _ _ (Right err)                 = (Right err)

evalBinary :: Value -> BinaryOp -> Value -> EvalResult
evalBinary (ValueInteger a) BinOpAdd  (ValueInteger b) = (Left(ValueInteger (a+b)))
evalBinary (ValueInteger a) BinOpSub  (ValueInteger b) = (Left(ValueInteger (a-b)))
evalBinary (ValueInteger a) BinOpMult (ValueInteger b) = (Left(ValueInteger (a*b)))
evalBinary (ValueInteger a) BinOpDiv  (ValueInteger b) = (Left(ValueInteger (a`div`b)))
evalBinary (ValueInteger a) BinOpPow  (ValueInteger b) = (Left(ValueInteger (a^b)))

evalBinary (ValueInteger a) BinOpEq   (ValueInteger b) = (Left(ValueBool (a == b)))
evalBinary (ValueInteger a) BinOpNeq  (ValueInteger b) = (Left(ValueBool (a /= b)))
evalBinary (ValueInteger a) BinOpGt   (ValueInteger b) = (Left(ValueBool (a > b)))
evalBinary (ValueInteger a) BinOpGeq  (ValueInteger b) = (Left(ValueBool (a >= b)))
evalBinary (ValueInteger a) BinOpLt   (ValueInteger b) = (Left(ValueBool (a < b)))
evalBinary (ValueInteger a) BinOpLeq  (ValueInteger b) = (Left(ValueBool (a <= b)))

evalBinary (ValueFloat a)   BinOpAdd  (ValueFloat b)   = (Left(ValueFloat (a+b)))
evalBinary (ValueFloat a)   BinOpSub  (ValueFloat b)   = (Left(ValueFloat (a-b)))
evalBinary (ValueFloat a)   BinOpMult (ValueFloat b)   = (Left(ValueFloat (a*b)))
evalBinary (ValueFloat a)   BinOpDiv  (ValueFloat b)   = (Left(ValueFloat (a/b)))
evalBinary (ValueFloat a)   BinOpPow  (ValueFloat b)   = (Left(ValueFloat (a**b)))

evalBinary (ValueFloat a) BinOpEq   (ValueFloat b) = (Left(ValueBool (a == b)))
evalBinary (ValueFloat a) BinOpNeq  (ValueFloat b) = (Left(ValueBool (a /= b)))
evalBinary (ValueFloat a) BinOpGt   (ValueFloat b) = (Left(ValueBool (a > b)))
evalBinary (ValueFloat a) BinOpGeq  (ValueFloat b) = (Left(ValueBool (a >= b)))
evalBinary (ValueFloat a) BinOpLt   (ValueFloat b) = (Left(ValueBool (a < b)))
evalBinary (ValueFloat a) BinOpLeq  (ValueFloat b) = (Left(ValueBool (a <= b)))

evalBinary (ValueInteger(a)) op (ValueFloat(b))    = evalBinary (ValueFloat $ fromIntegral(a)) op (ValueFloat b)
evalBinary (ValueFloat(a))   op (ValueInteger(b))  = evalBinary (ValueFloat a) op (ValueFloat $ fromIntegral(b))

evalBinary (ValueBool a) BinOpAnd  (ValueBool b) = (Left(ValueBool (a && b)))
evalBinary (ValueBool a) BinOpOr   (ValueBool b) = (Left(ValueBool (a || b)))
evalBinary (ValueBool a) BinOpEq   (ValueBool b) = (Left(ValueBool (a == b)))
evalBinary (ValueBool a) BinOpNeq  (ValueBool b) = (Left(ValueBool (a /= b)))


handleUnaryEvalResult :: UnaryOp -> EvalResult -> EvalResult
handleUnaryEvalResult op (Left a) = evalUnary op a
handleUnaryEvalResult _ (Right err) = (Right err)

evalUnary UnOpNot   (ValueBool a)       = (Left $ ValueBool    $ not    $ a)
evalUnary UnOpMinus (ValueInteger a)    = (Left $ ValueInteger $ negate $ a)
evalUnary UnOpPlus  (ValueInteger a)    = (Left $ ValueInteger $ negate $ a)
evalUnary UnOpMinus (ValueFloat a)      = (Left $ ValueFloat   $ negate $ a)
evalUnary UnOpPlus  (ValueFloat a)      = (Left $ ValueFloat   $ negate $ a)

evalUnary _ _           = (Right EvalErrorInvalidOp)
