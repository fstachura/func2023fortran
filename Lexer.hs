module Lexer (
    lexFull,
    defaultLexerState,
    token
) where

import Utils
import Debug.Trace

-- basic types

identifierChars = letters ++ digits

data Token = 
    TokenEqEq | TokenNeq | TokenLt | TokenLeq | TokenGt | TokenGeq |
    TokenAnd | TokenOr | 

    TokenLeftParen | TokenRightParen | TokenComma | TokenSemicolon | 
    TokenEq | 
    TokenPlus | TokenMinus | TokenStar | TokenSlash | TokenPow |

    TokenString(String) | TokenInteger(Integer) | TokenFloat(Double) | TokenBool(Bool) | 
    TokenIdentifier(String) |

    TokenEof
    deriving (Show)

-- lexing utils

---- consume chars while char != arg or char == newline, return consumed chars and the rest of the string
lexUntilOrNewline :: Char -> String -> Maybe(String, String)
lexUntilOrNewline _ "" = Nothing
lexUntilOrNewline char (r:rs)
    | (char == r) = Just("", rs)
    | (char == '\n') = Nothing
    | otherwise = case result of
        Just(s, rest) -> Just(r:s, rest)
        Nothing       -> result
    where result = (lexUntilOrNewline char rs)

---- consume chars while char != arg, return consumed chars and the rest of the string
lexUntil :: Char -> String -> Maybe(String, String)
lexUntil _ "" = Nothing
lexUntil char (r:rs)
    | (char == r) = Just("", rs)
    | otherwise = case result of
        Just(s, rest) -> Just(r:s, rest)
        Nothing       -> result
    where result = (lexUntil char rs)

---- consume chars while matcher contains char, return consumed chars and the rest of the string
lexWhile :: String -> String -> Maybe(String, String)
lexWhile matcher "" = Nothing
lexWhile matcher (x:xs)
    | (x `elem` matcher) = case result of
        Just(s, rest) -> Just(x:s, rest)
        Nothing       -> Just([x], xs)
    | otherwise      = Nothing
    where result = (lexWhile matcher xs)

-- lexing code

---- attempts to match string with a dotted identifier contents, returns Nothing on failure
matchDotToken :: String -> Maybe(Token, LexerStateUpdate)
matchDotToken "AND" = Just(TokenAnd, (posStateUpdate 3))
matchDotToken "OR" = Just(TokenOr, stateUpdate2)
matchDotToken "EQ" = Just(TokenEq, stateUpdate2)
matchDotToken "NE" = Just(TokenNeq, stateUpdate2)
matchDotToken "LT" = Just(TokenLt, stateUpdate2)
matchDotToken "LE" = Just(TokenLeq, stateUpdate2)
matchDotToken "GT" = Just(TokenGt, stateUpdate2)
matchDotToken "GE" = Just(TokenGeq, stateUpdate2)
matchDotToken "TRUE" = Just(TokenBool(True), (posStateUpdate 4))
matchDotToken "T" = Just(TokenBool(True), stateUpdate1)
matchDotToken "FALSE" = Just(TokenBool(False), (posStateUpdate 5))
matchDotToken "F" = Just(TokenBool(False), stateUpdate1)
matchDotToken _ = Nothing

---- attempts to consume an integer or a float, returns Nothing on failure
lexNumber :: String -> Maybe(Token, String, LexerStateUpdate)
lexNumber str = case result of
        Just(a, '.':rs) -> 
            maybeOr 
                (Just(TokenInteger(read a), '.':rs, (posStateUpdate (length a))))
                (\(b, rs) -> Just(TokenFloat(
                    read (a ++ "." ++ b)), 
                    rs, 
                    (posStateUpdate ((length a)+1+(length b)))
                )) 
                (lexWhile digits rs) 
        Just(a, rest) -> 
            Just(TokenInteger(read a), rest, (posStateUpdate (length a)))
        Nothing -> Nothing
    where result = lexWhile digits str

---- attempts to consume a dotted identifier/operator (.TRUE., .EQ., ...)
lexDotId :: String -> Maybe(Token, String, LexerStateUpdate)
lexDotId xs = case (lexUntilOrNewline '.' xs) of
    Just(id, rest) -> 
        maybeOrNothing 
            (\(x,u) -> Just(x, rest, u)) 
            (matchDotToken id) 
    Nothing -> Nothing

---- attempts to consume a token, returns Nothing on failure
lexToken :: String -> Maybe(Token, String, LexerStateUpdate)
lexToken ('.':xs) = lexDotId xs
lexToken ('"':xs) = 
    maybeOrNothing
        (\(s,r) -> Just(TokenString(s), r, (strStateUpdate s))) 
        (lexUntilOrNewline '"' xs) 

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
lexToken ('\n':xs)    = Just(TokenSemicolon, xs, stateUpdate1)

lexToken (' ':xs) = case (lexToken xs) of
    Just(token, rest, update) -> 
        Just(token, rest, (addUpdates stateUpdate1 update))
    Nothing -> Nothing

lexToken (x:xs) 
    | x `elem` digits = lexNumber (x:xs)
    | x `elem` letters = 
        maybeOr
            (Just(TokenIdentifier([x]), xs, stateUpdate1))
            (\(s,r) -> Just(TokenIdentifier(x:s), r, (posStateUpdate ((length s)+1)))) 
            (lexWhile identifierChars xs) 

lexToken ('!':xs) = 
    maybeOr
        (Just(TokenSemicolon, "", lineStateUpdate))
        (\(s,r) -> Just(TokenSemicolon, r, lineStateUpdate))
        (lexUntil '\n' xs)

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
    | (updateLine stateUpdate) > (stateLine curState) = 
        state (updatePos stateUpdate) (updateLine stateUpdate) 
    | otherwise = 
        state ((statePos curState)+(updatePos stateUpdate)) (stateLine curState) 

defaultLexerState = state 0 0 

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
    | otherwise     = stateUpdate (apos+bpos) aline 

-- token wrapper

data TokenWithInfo = TokenWithInfo { 
    token :: Token,
    lexerState :: LexerState
}
    deriving (Show)

-- entry

data LexingResult = Ok | Error
    deriving (Show)

---- maps string into a list of tokens. returns successfully consumed tokens, second value is Error on failure
lexFull :: LexerState -> String -> ([TokenWithInfo], LexingResult)
lexFull state s = case (lexToken s) of
    Just(TokenEof, _, stateUpdate) -> ([TokenWithInfo{token=TokenEof, lexerState=state}], Ok)
    Just(t, rest, stateUpdate) -> 
        case (lexFull (updateState state stateUpdate) rest) of
            (tokens, Ok) -> (c ++ tokens, Ok)
            (tokens, Error) -> (c ++ tokens, Error)
        where c = [TokenWithInfo{token=t, lexerState=state}]
    Nothing -> ([], Error)

