import System.Environment
import System.IO
import Ast.AstTypes
import Eval.Eval
import Eval.GotoMap
import Utils.Map
import Utils.Utils
import Lexer
import Parser

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
            case (snd tokens) of
                Ok -> 
                    let parsed = (program $ newParserState $ fst tokens) in
                    case parsed of 
                        Right(StmtBlockType(stmts), _) -> do
                            putStrLn $ show $ parsed
                            let execResult = (execBlock (evalContext (createGotoMap stmts simpleMap)) stmts) 
                            execResult >>=
                                \result ->
                                    case result of
                                        Left(err) -> do
                                            putStrLn ""
                                            putStrLn $ show $ err
                                            return ()
                                        Right(_) -> do
                                            putStrLn "ok"
                                            return ()
                        Left(_) -> do
                            mapM_ putStrLn (map (show . token) (fst tokens))
                            putStrLn $ show $ parsed
                            putStrLn "parsing error"
                Error -> do
                    mapM_ putStrLn (map (show . token) (fst tokens))
                    putStrLn "lexing error"

            --case parsed of
            --    Right(expr) -> putStrLn $ show $ (eval defaultEvalContext expr)
     

