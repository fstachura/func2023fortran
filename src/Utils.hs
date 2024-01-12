module Utils (
    maybeOr,
    lowercaseLetters,
    uppercaseLetters,
    letters,
    digits,
    altM,
    altE,
    strToLower
) where

altM :: (Maybe a) -> (Maybe a) -> (Maybe a)
altM (Just a) _ = (Just a)
altM _ b = b

altE :: (Either a b) -> (Either a b) -> (Either a b)
altE (Right a) _ = (Right a)
altE _ b = b

maybeOr :: b -> (a -> b) -> (Maybe a) -> b
maybeOr _ f (Just(a)) = f a
maybeOr b _ Nothing = b

--mapLeft :: (a -> c) -> Either a b -> c
--mapLeft f (Left a)  = f a
--mapLeft _ b         = b

lowercaseLetters = "abcdefghijklmnoprstquwxyz"
uppercaseLetters = "ABCDEFGHIJKLMNOPRSTQUWXYZ"
letters = lowercaseLetters ++ uppercaseLetters
digits = "0123456789"

toLower :: Char -> Char
toLower c 
    | (o >= 65 && o <= 90)  = toEnum (o + 32)
    | otherwise             = c
    where o = fromEnum c

strToLower :: String -> String
strToLower s = map toLower s

