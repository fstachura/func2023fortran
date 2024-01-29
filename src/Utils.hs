module Utils (
    maybeOr,
    lowercaseLetters,
    uppercaseLetters,
    letters,
    digits,
    altM,
    altE,
    foldME,
    nothingOnLeft,
    leftOnNothing,
    flattenME,
    lookupFilter,
    strToLower,
    count,
    prefix
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

foldME :: (b -> c -> (Either a b)) -> b -> [c] -> (Either a b)
foldME _ d [] = Right(d)
foldME f d (e:el) = foldME' f (f d e) el

foldME' :: (b -> c -> (Either a b)) -> (Either a b) -> [c] -> (Either a b)
foldME' f (res@(Left _)) _ = res
foldME' f (Right b) (e:el) = foldME' f (f b e) el
foldME' _ res [] = res

nothingOnLeft :: (Either a b) -> (Maybe b)
nothingOnLeft (Left _) = Nothing
nothingOnLeft (Right b) = Just $ b

leftOnNothing :: a -> (Maybe b) -> (Either a b)
leftOnNothing _ (Just b)  = return $ b
leftOnNothing a (Nothing) = Left(a)

flattenME :: a -> (Maybe (Either a b)) -> (Either a b)
flattenME _ (Just e) = e
flattenME a Nothing  = Left $ a

-- list utils

count :: (a -> Bool) -> [a] -> Int
count f = foldl (\acc el -> if (f el) then acc+1 else acc) 0

prefix :: (Eq a) => [a] -> [a] -> Bool
prefix m s = (take (length m) s) == m

lookupFilter :: (a -> Bool) -> [(a, b)] -> Maybe (a, b)
lookupFilter _ [] = Nothing
lookupFilter f ((a, b):xs) = if (f a) then Just $ (a, b) else lookupFilter f xs

-- string utils

lowercaseLetters = "abcdefghijklmnoprstquwvxyz"
uppercaseLetters = "ABCDEFGHIJKLMNOPRSTQUWVXYZ"
letters = lowercaseLetters ++ uppercaseLetters
digits = "0123456789"

toLower :: Char -> Char
toLower c 
    | (o >= 65 && o <= 90)  = toEnum (o + 32)
    | otherwise             = c
    where o = fromEnum c

strToLower :: String -> String
strToLower s = map toLower s

