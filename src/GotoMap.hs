module GotoMap (
    GotoMap,
    createGotoMap
) where

import AstTypes
import Map

type GotoMap = SimpleMap (Namespace, Int) [Stmt]

createGotoMap :: [Stmt] -> GotoMap -> GotoMap
createGotoMap [] = id
createGotoMap (stmt@(StmtLabeled (lt, l) _):stmts) = createGotoMap stmts . (mapInsert (lt, l) (stmt:stmts))
createGotoMap (_:stmts) = createGotoMap stmts
