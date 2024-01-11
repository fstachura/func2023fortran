module Eval (
    eval,
    defaultEvalContext
) where

import AstTypes
import Map

data Value = ValueInteger(Integer) | ValueString(String) | ValueFloat(Double) | ValueBool(Bool)
    deriving(Show)

data EvalError = EvalErrorUnknownVariable | EvalErrorUnmatchedTypes | EvalErrorInvalidOp | EvalErrorNotImplemented
    deriving(Show)

data EvalContext = EvalContext { }
    deriving(Show)

defaultEvalContext = EvalContext {  }

type EvalResult = (Either EvalError Value)

eval :: EvalContext -> Expr -> EvalResult

eval ctx (ExprBin(a, op, b))    = handleBinaryEvalResults (eval ctx a) op (eval ctx b)
eval ctx (ExprUn(op, a))        = handleUnaryEvalResult op (eval ctx a)
eval ctx (ExprString(val))      = (Right(ValueString(val)))
eval ctx (ExprInteger(val))     = (Right(ValueInteger(val)))
eval ctx (ExprFloat(val))       = (Right(ValueFloat(val)))
eval ctx (ExprBool(val))        = (Right(ValueBool(val)))
-- eval _ ExprIdentifier(String) = 


handleBinaryEvalResults :: EvalResult -> BinaryOp -> EvalResult -> EvalResult
handleBinaryEvalResults (Right a) op (Right b)            = evalBinary a op b 
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
