module AstTypes (
    Expr(..),
    BinaryOp(..),
    UnaryOp(..),
    StmtBlockType(..),
    Namespace(..),
    Stmt(..)
) where

import Map

data BinaryOp = BinOpEq | BinOpNeq | BinOpGt | BinOpGeq | BinOpLt | BinOpLeq | 
                BinOpAdd | BinOpSub | BinOpMult | BinOpDiv | BinOpPow | 
                BinOpAnd | BinOpOr

instance Show BinaryOp where
    show BinOpEq = ".EQV."
    show BinOpNeq = ".NEQ."
    show BinOpGt = ">"
    show BinOpGeq = ">="
    show BinOpLt = "<"
    show BinOpLeq = "<="
    show BinOpAdd = "+"
    show BinOpSub = "-"
    show BinOpMult = "*"
    show BinOpDiv = "/"
    show BinOpPow = "**"
    show BinOpAnd = ".AND."
    show BinOpOr = ".OR."

data UnaryOp = UnOpNot | UnOpMinus | UnOpPlus

instance Show UnaryOp where
    show UnOpNot = ".NOT."
    show UnOpMinus = "-"
    show UnOpPlus = "+"

data Expr = 
    ExprBin(Expr, BinaryOp, Expr) |
    ExprUn(UnaryOp, Expr) |
    ExprString(String) |
    ExprInteger(Integer) |
    ExprFloat(Double) |
    ExprBool(Bool) |
    ExprIdentifier(Namespace, String)

instance Show Expr where
    show (ExprBin(a, op, b)) = "(" ++ (show a) ++ " " ++ (show op) ++ " " ++ (show b) ++ ")"
    show (ExprUn(op, a)) = "(" ++ (show op) ++ (show a) ++ ")"
    show (ExprString(str)) = (show str)
    show (ExprInteger(int)) = (show int)
    show (ExprFloat(f)) = (show f)
    show (ExprBool(b)) = (show b)
    show (ExprIdentifier(n, str)) = (show n) ++ ":" ++ str

data StmtBlockType = StmtBlockType([Stmt])

instance Show StmtBlockType where
    show (StmtBlockType(stmts)) = foldl (\acc stmt -> acc ++ (show stmt) ++ "\n") "" stmts
    
data IfBlockType = IfBlockType(Expr, StmtBlockType, Maybe IfBlockType)

data Namespace = NamespaceVisible | NamespaceIf | NamespaceDo
    deriving(Eq, Ord)

instance Show Namespace where
    show NamespaceVisible = "nex"
    show NamespaceIf = "nif"
    show NamespaceDo = "ndo"

data Stmt = 
    StmtAssign((Namespace, String), Expr) | 
    StmtWrite([Expr]) | 
    StmtRead([String]) |
    StmtLabeled(Namespace, Integer, Stmt) |
    StmtNoop |

    StmtIntCompiledIf(Expr, Namespace, Integer) |
    StmtAbsoluteGoto(Namespace, Integer) |
    StmtArithmeticIf(Expr, Integer, Integer, Integer) |
    StmtComputedGoto([Integer], Expr)

instance Show Stmt where
    show (StmtIntCompiledIf(expr, namespace, label)) = "cif (" ++ (show expr) ++ ") -> " ++ 
        (show namespace) ++ ":" ++ (show label)
    show (StmtAssign((n, str), expr)) = (show n) ++ ":" ++ str ++ " = " ++ (show expr)
    show (StmtAbsoluteGoto(t, label)) = "goto " ++ (show t) ++ " " ++ (show label)
    show (StmtComputedGoto(labels, expr)) = 
        "goto (" ++ 
        (foldl (\acc l -> acc ++ (show l) ++ ", ") "" labels) ++ 
        ") " ++ 
        (show expr)
    show (StmtArithmeticIf(expr, a, b, c)) = 
        "if (" ++ (show expr) ++ ") " ++ (show a) ++ ", " ++ (show b) ++ ", " ++ (show c)
    show (StmtWrite(exprs)) = 
        "write (*,*)" ++ 
        (foldl (\acc e -> acc ++ ", " ++ (show e)) "" exprs)
    show (StmtRead(strs)) =
        "read (*,*)" ++ 
        (foldl (\acc i -> acc ++ ", " ++ i) "" strs)
    show (StmtLabeled(lt, l, stmt)) =
        (show lt) ++ (show l) ++ " " ++ (show stmt)
    show (StmtNoop) = "noop"

