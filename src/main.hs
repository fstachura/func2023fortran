import System.Environment
import System.IO
import Lexer
import Parser
import Utils
import Eval
import GotoMap
import Map
import AstTypes

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
            case (snd tokens) of
                Ok -> 
                    let parsed = (program $ newParserState $ fst tokens) in
                    case parsed of 
                        Right(StmtBlockType(stmts), _) -> do
                            let execResult = (execBlock (evalContext (createGotoMap stmts simpleMap)) stmts) 
                            execResult >>=
                                putStrLn . show 
                        Left(_) -> putStrLn $ show $ parsed
                Error ->
                    putStrLn "lexing error"

            --case parsed of
            --    Right(expr) -> putStrLn $ show $ (eval defaultEvalContext expr)
     

