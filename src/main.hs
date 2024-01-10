import System.Environment
import System.IO
import Lexer
import Parser
import Utils
import Eval

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
            let parsed = (expression $ newParserState $ fst tokens) >>= (Right . fst)
            putStrLn $ show $ parsed
            case parsed of
                Right(expr) -> putStrLn $ show $ (eval defaultEvalContext expr)
     

