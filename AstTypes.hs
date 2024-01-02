module AstTypes (
    Expr(..),
    Stmt(..)
) where

data Expr = 
    ExprAnd(Expr, Expr) | 
    ExprOr(Expr, Expr) | 
    ExprNot(Expr) | 

    ExprEq(Expr, Expr) | 
    ExprNeq(Expr, Expr) | 
    ExprGt(Expr, Expr) | 
    ExprGeq(Expr, Expr) | 
    ExprLt(Expr, Expr) | 
    ExprLeq(Expr, Expr) | 

    ExprAdd(Expr, Expr) |
    ExprSub(Expr, Expr) |
    ExprMult(Expr, Expr) |
    ExprDiv(Expr, Expr) |
    ExprPow(Expr, Expr) |

    ExprInteger(Integer) |
    ExprFloat(Double) |
    ExprIdentifier(String) |
    ExprString(String) |
    ExprBool(Bool)

data Stmt = 
    StmtAssign(String, Expr) | 
    StmtLoop(String, Expr, Expr, Expr, Stmt) |
    StmtAbsoluteGoto(Integer) |
    StmtComputedGoto([Integer], Expr) |
    StmtArithmeticIf(Expr, Integer, Integer, Integer) |
    StmtIf(Expr, Stmt, Stmt) |
    StmtPrint([Expr]) | 
    StmtRead([Expr]) |
    Stmt(Stmt)

