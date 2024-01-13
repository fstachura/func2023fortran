module Lexer (
    lexFull,
    defaultLexerState,
    token,
    LexingResult(..)
) where

import Utils
import TokenTypes
import Debug.Trace

identifierChars = letters ++ digits

-- lexing utils

nothingIfFirstEmpty :: (String, b) -> Maybe(String, b)
nothingIfFirstEmpty ("", _) = Nothing
nothingIfFirstEmpty a = (Just a)

advance :: (String, String) -> (String, String)
advance (a, b) = (a, drop 1 b)

---- consume chars while char != arg, return consumed chars and the rest of the string
lexUntil :: String -> String -> Maybe(String, String)
lexUntil matcher = nothingIfFirstEmpty . (break (flip (elem) matcher))

---- consume chars while matcher contains char, return consumed chars and the rest of the string
lexWhile :: String -> String -> Maybe(String, String)
lexWhile matcher = nothingIfFirstEmpty . (span (flip (elem) matcher))

-- lexing code

---- attempts to match string with a dotted identifier contents, returns Nothing on failure
matchDotToken :: String -> Maybe(Token, LexerStateUpdate)
matchDotToken "AND" = Just(TokenAnd, (posStateUpdate 3))
matchDotToken "OR" = Just(TokenOr, stateUpdate2)
matchDotToken "NOT" = Just(TokenNot, (posStateUpdate 3))
matchDotToken "EQ" = Just(TokenEq, stateUpdate2)
matchDotToken "NE" = Just(TokenNeq, stateUpdate2)
matchDotToken "EQV" = Just(TokenEqv, (posStateUpdate 3))
matchDotToken "NEQV" = Just(TokenNeqv, (posStateUpdate 4))

matchDotToken "LT" = Just(TokenLt, stateUpdate2)
matchDotToken "LE" = Just(TokenLeq, stateUpdate2)
matchDotToken "GT" = Just(TokenGt, stateUpdate2)
matchDotToken "GE" = Just(TokenGeq, stateUpdate2)
matchDotToken "TRUE" = Just(TokenBool(True), (posStateUpdate 4))
matchDotToken "T" = Just(TokenBool(True), stateUpdate1)
matchDotToken "FALSE" = Just(TokenBool(False), (posStateUpdate 5))
matchDotToken "F" = Just(TokenBool(False), stateUpdate1)
matchDotToken _ = Nothing

---- TODO consume -/+
---- attempts to consume an integer or a float, returns Nothing on failure
lexNumber :: String -> Maybe(Token, String, LexerStateUpdate)
lexNumber str = 
    (lexWhile digits str) >>=
    \result -> case result of
        (a, '.':rs) -> 
            ((lexWhile digits rs) >>= 
                \(b, rs) -> Just(TokenFloat(
                    read (a ++ "." ++ b)), 
                    rs, 
                    (posStateUpdate ((length a)+1+(length b)))
                )) `altM`
                    Just(TokenInteger(read a), '.':rs, (posStateUpdate (length a)))
        (a, rest) -> 
            Just(TokenInteger(read a), rest, (posStateUpdate (length a)))

---- attempts to consume a dotted identifier/operator (.TRUE., .EQ., ...)
lexDotId :: String -> Maybe(Token, String, LexerStateUpdate)
lexDotId xs = 
    (lexUntil ".\n" xs) >>=
    \(id, r:rest) -> (matchDotToken id) >>= 
        \(token, u) -> Just(token, rest, (addUpdates u stateUpdate1))

---- attempts to consume a token, returns Nothing on failure
lexToken :: String -> Maybe(Token, String, LexerStateUpdate)
lexToken ('.':xs) = lexDotId xs
lexToken ('"':xs) = 
    (lexUntil "\"\n" xs) >>=
        \(s,r) -> Just(TokenString(s), (drop 1 r), (strStateUpdate s)) 

lexToken ('<':'=':xs) = Just(TokenLeq, xs, stateUpdate2)
lexToken ('>':'=':xs) = Just(TokenGeq, xs, stateUpdate2)
lexToken ('=':'=':xs) = Just(TokenEqEq, xs, stateUpdate2)
lexToken ('/':'=':xs) = Just(TokenNeq, xs, stateUpdate2)
lexToken ('!':'=':xs) = Just(TokenNeq, xs, stateUpdate2)
lexToken ('*':'*':xs) = Just(TokenPow, xs, stateUpdate2)
lexToken ('+':xs)     = Just(TokenPlus, xs, stateUpdate1)
lexToken ('-':xs)     = Just(TokenMinus, xs, stateUpdate1)
lexToken ('*':xs)     = Just(TokenStar, xs, stateUpdate1)
lexToken ('/':xs)     = Just(TokenSlash, xs, stateUpdate1)
lexToken ('=':xs)     = Just(TokenEq, xs, stateUpdate1)
lexToken ('>':xs)     = Just(TokenGt, xs, stateUpdate1)
lexToken ('<':xs)     = Just(TokenLt, xs, stateUpdate1)
lexToken ('(':xs)     = Just(TokenLeftParen, xs, stateUpdate1)
lexToken (')':xs)     = Just(TokenRightParen, xs, stateUpdate1)
lexToken (',':xs)     = Just(TokenComma, xs, stateUpdate1)
lexToken (';':xs)     = Just(TokenSemicolon, xs, stateUpdate1)
lexToken ('\n':xs)    = Just(TokenSemicolon, xs, lineStateUpdate)

lexToken (' ':xs) = 
    (lexToken xs) >>=
        \(token, rest, update) -> 
            Just(token, rest, (addUpdates stateUpdate1 update))

lexToken (x:xs) 
    | x `elem` digits = lexNumber (x:xs)
    | x `elem` letters = 
        ((lexWhile identifierChars xs) >>=
            \(s,r) -> Just(TokenIdentifier(x:s), r, (posStateUpdate ((length s)+1)))) `altM`
            (Just(TokenIdentifier([x]), xs, stateUpdate1))
            

lexToken ('!':xs) = 
    ((lexUntil "\n" xs) >>= \(s,r) -> Just(TokenSemicolon, (drop 1 r), lineStateUpdate)) `altM`
        (Just(TokenSemicolon, (drop 1 xs), lineStateUpdate))
            
lexToken ""           = Just(TokenEof, "", emptyStateUpdate)
lexToken _            = Nothing

-- state 

data LexerState = LexerState {
    statePos  :: Int,
    stateLine :: Int
}
    deriving (Show)

state pos line = LexerState { statePos=pos, stateLine=line }

updateState :: LexerState -> LexerStateUpdate -> LexerState
updateState curState stateUpdate 
    | (updateLine stateUpdate) >= 1 =
        state 0 ((stateLine curState)+(updateLine stateUpdate))
    | otherwise = 
        state ((statePos curState)+(updatePos stateUpdate)) (stateLine curState)

defaultLexerState = state 0 1 

-- state update

data LexerStateUpdate = LexerStateUpdate {
    updatePos  :: Int,
    updateLine :: Int
}
    deriving (Show)

stateUpdate pos line = LexerStateUpdate { updatePos=pos, updateLine=line }

posStateUpdate = (flip stateUpdate) 0

emptyStateUpdate = stateUpdate 0 0 
stateUpdate1 = posStateUpdate 1
stateUpdate2 = posStateUpdate 2

strStateUpdate str = posStateUpdate (2+(length str))
lineStateUpdate = stateUpdate 0 1

addUpdates :: LexerStateUpdate -> LexerStateUpdate -> LexerStateUpdate
addUpdates LexerStateUpdate { updatePos=apos, updateLine=aline } 
           LexerStateUpdate { updatePos=bpos, updateLine=bline }
    | bline > aline = stateUpdate bpos bline
    | aline > bline = stateUpdate apos aline
    | otherwise     = stateUpdate (max apos bpos) aline 

stateToLocation s = (newTokenLocation (statePos s) (stateLine s))

-- entry

data LexingResult = Ok | Error
    deriving (Show)

---- maps string into a list of tokens. returns successfully consumed tokens, second value is Error on failure
lexFull :: LexerState -> String -> ([TokenWithInfo], LexingResult)
lexFull state s = case (lexToken s) of
    Just(TokenEof, _, stateUpdate) -> 
        ([TokenWithInfo{token=TokenEof, tokenLocation=(stateToLocation state)}], Ok)
    Just(t, rest, stateUpdate) -> 
        case (lexFull (updateState state stateUpdate) rest) of
            (tokens, Ok) -> (c:tokens, Ok)
            (tokens, Error) -> (c:tokens, Error)
        where c = TokenWithInfo{token=t, tokenLocation=(stateToLocation state)}
    Nothing -> ([], Error)

