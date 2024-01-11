module AstTypes (
    Expr(..),
    BinaryOp(..),
    UnaryOp(..),
    GoToMap,
    StmtBlockType,
    Stmt(..)
) where

import Map

data BinaryOp = BinOpEq | BinOpNeq | BinOpGt | BinOpGeq | BinOpLt | BinOpLeq | 
                BinOpAdd | BinOpSub | BinOpMult | BinOpDiv | BinOpPow | 
                BinOpAnd | BinOpOr
    deriving(Show)

data UnaryOp = UnOpNot | UnOpMinus | UnOpPlus
    deriving(Show)

data Expr = 
    ExprBin(Expr, BinaryOp, Expr) |
    ExprUn(UnaryOp, Expr) |
    ExprString(String) |
    ExprInteger(Integer) |
    ExprFloat(Double) |
    ExprBool(Bool) |
    ExprIdentifier(String)
    deriving(Show)

type GoToMap = (SimpleMap Integer [Stmt])

type StmtBlockType = ([Stmt], GoToMap)

data Stmt = 
    StmtAssign(String, Expr) | 
    StmtLoop(String, Expr, Expr, Expr, Stmt) |
    StmtAbsoluteGoto(Integer) |
    StmtComputedGoto([Integer], Expr) |
    StmtArithmeticIf(Expr, Integer, Integer, Integer) |
    StmtIf(Expr, Stmt, Stmt) |
    StmtComputedIf(Expr, Integer, Integer, Integer) |
    StmtWrite([Expr]) | 
    StmtRead([String]) |
    StmtProgram(StmtBlockType) |
    StmtLabeled(Integer, Stmt) | 
    StmtBlock(StmtBlockType)
    deriving(Show)

