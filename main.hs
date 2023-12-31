import Lexer
import System.Environment
import System.IO

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

-- goto map 
-- ifs and dos push their own goto map so that jumping into ifs is impossible
-- variable map (global for a program for now)

main = do
    args <- getArgs
    if ((length args) /= 1) then do
        name <- getProgName
        putStrLn ("usage: " ++ name ++ " filename.f")
    else do
        let filename = head args 
        withFile filename ReadMode $ \h -> do
            src <- (hGetContents h)
            let tokens = (lexFull defaultLexerState src)
            mapM_ putStrLn (map (show . token) (fst tokens))
            putStrLn (show (snd tokens))
     

