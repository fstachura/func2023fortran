module EvalTypes (
    Value(..),
    EvalError(..),
    EvalContext(..),
    EvalResult,
    Function,
    VariableMap,
    FunctionMap
) where

import AstTypes
import Map
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
    EvalErrorUnknownVariable((Namespace, String)) | 
    EvalErrorIncompatibleTypes | 
    EvalErrorInvalidOp | 
    EvalErrorLabelNotFound(Integer) | 
    EvalErrorReading(String) | 
    EvalErrorInvalidFunctionArgs(String, [Value]) | 
    EvalErrorUnknownFunction(String) | 
    EvalErrorNotImplemented
    deriving(Show)

type Function = [Value] -> EvalContext -> EvalResult

type VariableMap = (SimpleMap (Namespace, String) Value)
type FunctionMap = (SimpleMap String Function)

data EvalContext = EvalContext { 
    variableMap :: VariableMap,
    gotoMap :: GotoMap,
    functionMap :: FunctionMap
}

instance Show EvalContext where
    show EvalContext { variableMap=vm, gotoMap=gm } = 
        "EvalContext{ variableMap=" ++ (show vm) ++ ", gotoMap=" ++ (show gm) ++ " }"

type EvalResult = (Either EvalError Value)

