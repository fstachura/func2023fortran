module Utils (
    maybeOr,
    maybeOrNothing,
    lowercaseLetters,
    uppercaseLetters,
    letters,
    digits,
) where

maybeOr :: b -> (a -> b) -> (Maybe a) -> b
maybeOr _ f (Just(a)) = f a
maybeOr b _ Nothing = b

maybeOrNothing = maybeOr Nothing

lowercaseLetters = "abcdefghijklmnoprstquwxyz"
uppercaseLetters = "ABCDEFGHIJKLMNOPRSTQUWXYZ"
letters = lowercaseLetters ++ uppercaseLetters
digits = "0123456789"
