module Lexer (
    lexFull,
    defaultLexerState,
    token,
    LexingResult(..)
) where

import Utils
import TokenTypes
import Debug.Trace

-- lexing utils

-- utility function to map empty string values to Nothing
nothingIfFirstEmpty :: (String, b) -> Maybe (String, b)
nothingIfFirstEmpty ("", _) = Nothing
nothingIfFirstEmpty a       = Just a

---- consume chars while matcher does not contain char, return consumed chars and the rest of the string
lexUntil :: String -> String -> Maybe (String, String)
lexUntil matcher = nothingIfFirstEmpty . break (flip (elem) matcher)

---- consume chars while matcher contains char, return consumed chars and the rest of the string
lexWhile :: String -> String -> Maybe (String, String)
lexWhile matcher = nothingIfFirstEmpty . span (flip (elem) matcher)

-- lexing code

---- attempts to match string with a dotted identifier contents, returns Nothing on failure
matchDotToken :: String -> Maybe (Token, LexerStateUpdate)
matchDotToken s = 
    (lookup (strToLower s) dotTokenMap) >>= 
    \token -> Just $ (token, posStateUpdate $ length s)

---- attempts to consume an integer or a float, returns Nothing on failure
lexNumber :: String -> Maybe(Token, String, LexerStateUpdate)
lexNumber str = do
    result <- lexWhile digits str
    case result of
        (a, '.':rs) -> 
            ((lexWhile digits rs) >>= 
                \(b, rs) -> return $ (
                    TokenFloat $ read $ a ++ "." ++ b, 
                    rs, 
                    posStateUpdate $ (length a)+1+(length b))
            ) `altM`
            return (TokenInteger $ read a, '.':rs, posStateUpdate $ length a)
        (a, rest) -> 
            return (TokenInteger $ read a, rest, posStateUpdate $ length a)

---- attempts to consume a dotted identifier/operator (.TRUE., .EQ., ...)
lexDotId :: String -> Maybe(Token, String, LexerStateUpdate)
lexDotId xs = 
    (lexUntil ".\n" xs) >>=
    \(id, _:rest) -> (matchDotToken id) >>= 
        \(token, u) -> Just (token, rest, (addUpdates u stateUpdate1))

---- attempts to consume a token, returns Nothing on failure
lexToken :: String -> Maybe(Token, String, LexerStateUpdate)

lexToken ('.':xs) = lexDotId xs

lexToken ('"':xs) = 
    (lexUntil "\"\n" xs) >>=
        \(s,r) -> Just (TokenString s, drop 1 r, strStateUpdate s)

lexToken (' ':xs) = 
    (lexToken xs) >>=
        \(token, rest, update) -> 
            Just (token, rest, addUpdates stateUpdate1 update)

lexToken ('\n':xs) = Just (TokenSemicolon, xs, lineStateUpdate)

lexToken (x:xs) 
    | x `elem` digits = lexNumber (x:xs)
    | x `elem` letters = 
        ((lexWhile identifierChars xs) >>=
            \(s,r) -> Just(TokenIdentifier(x:s), r, (posStateUpdate ((length s)+1)))) `altM`
            (Just (TokenIdentifier [x], xs, stateUpdate1))

lexToken ('!':xs) = 
    ((lexUntil "\n" xs) >>= \(_,r) -> Just (TokenSemicolon, drop 1 r, lineStateUpdate)) `altM`
        (Just (TokenSemicolon, drop 1 xs, lineStateUpdate))
            
lexToken "" = Just(TokenEof, "", emptyStateUpdate)

lexToken s =
    (lookupFilter ((flip prefix) s) simpleTokenMap) >>= 
        \(tokenStr, token) -> 
        let l = (length tokenStr) in
            Just $ (token, drop l s, posStateUpdate l)

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
    Just(TokenEof, _, _) ->
        ([TokenWithInfo{token=TokenEof, tokenLocation=(stateToLocation state)}], Ok)
    Just(t, rest, stateUpdate) -> 
        case (lexFull (updateState state stateUpdate) rest) of
            (tokens, Ok) -> (c:tokens, Ok)
            (tokens, Error) -> (c:tokens, Error)
        where c = TokenWithInfo{token=t, tokenLocation=(stateToLocation state)}
    Nothing -> ([], Error)

