module BuiltinFunctions (
    defaultFunctionMap
) where

import Map
import EvalTypes

defaultFunctionMap = [
        ("mod", modFunction),
        ("sqrt", d2dFunction sqrt "sqrt"),
        ("sin", d2dFunction sin "sin"),
        ("cos", d2dFunction cos "cos"),
        ("tan", d2dFunction tan "tan"),
        ("asin", d2dFunction asin "asin"),
        ("acos", d2dFunction acos "acos"),
        ("atan", d2dFunction atan "atan"),
        ("sinh", d2dFunction sinh "sinh"),
        ("cosh", d2dFunction cosh "cosh"),
        ("tanh", d2dFunction tanh "tanh"),
        ("asinh", d2dFunction asinh "asinh"),
        ("acosh", d2dFunction acosh "acosh"),
        ("atanh", d2dFunction atanh "atanh"),
        ("exp", d2dFunction exp "exp"),
        ("log", d2dFunction exp "log"),
        ("floor", d2iFunction floor "floor"),
        ("ceil", d2iFunction ceiling "ceil"),
        ("ceil", d2iFunction round "round"),
        ("abs", absFunction),
        ("max", maxFunction),
        ("min", minFunction),
        ("real", realFunction)
    ]

modFunction :: Function
modFunction [ValueInteger(a), ValueInteger(b)] _ = Right $ ValueInteger $ a `mod` b
modFunction args _ = Left $ EvalErrorInvalidFunctionArgs "mod" args

realFunction :: Function
realFunction [ValueInteger(a)] _ = Right $ ValueFloat $ fromIntegral a
realFunction [ValueFloat(a)] _ = Right $ ValueFloat a
realFunction args _ = Left $ EvalErrorInvalidFunctionArgs "real" args

-- cant generalize over these three, not sure why
absFunction :: Function
absFunction [ValueFloat(a)] _ = Right $ ValueFloat $ abs a
absFunction [ValueInteger(a)] _ = Right $ ValueInteger $ abs a
absFunction args _ = Left $ EvalErrorInvalidFunctionArgs "abs" args

maxFunction [ValueFloat(a), ValueFloat(b)] _ = Right $ ValueFloat $ max a b
maxFunction [ValueInteger(a), ValueInteger(b)] _ = Right $ ValueInteger $ max a b
maxFunction [ValueFloat(a), ValueInteger(b)] _ = Right $ ValueFloat $ max a (fromIntegral b)
maxFunction [ValueInteger(a), ValueFloat(b)] _ = Right $ ValueFloat $ max (fromIntegral a) b
maxFunction args _ = Left $ EvalErrorInvalidFunctionArgs "max" args

minFunction [ValueFloat(a), ValueFloat(b)] _ = Right $ ValueFloat $ min a b
minFunction [ValueInteger(a), ValueInteger(b)] _ = Right $ ValueInteger $ min a b
minFunction [ValueFloat(a), ValueInteger(b)] _ = Right $ ValueFloat $ min a (fromIntegral b)
minFunction [ValueInteger(a), ValueFloat(b)] _ = Right $ ValueFloat $ min (fromIntegral a) b
minFunction args _ = Left $ EvalErrorInvalidFunctionArgs "min" args

d2iFunction :: (Double -> Int) -> String -> Function
d2iFunction f _ [ValueFloat(a)] _ = Right $ ValueInteger $ f a
d2iFunction _ n args _ = Left $ EvalErrorInvalidFunctionArgs n args

d2dFunction :: (Double -> Double) -> String -> Function
d2dFunction f _ [ValueFloat(a)] _ = Right $ ValueFloat $ f a
d2dFunction f _ [ValueInteger(a)] _ = Right $ ValueFloat $ f $ fromIntegral $ a
d2dFunction _ name args _ = Left $ EvalErrorInvalidFunctionArgs name args

