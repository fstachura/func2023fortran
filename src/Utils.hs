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

foldME :: (b -> c -> (Either a b)) -> b -> [c] -> (Either a b)
foldME _ d [] = Right(d)
foldME f d (e:el) = foldME' f (f d e) el

foldME' :: (b -> c -> (Either a b)) -> (Either a b) -> [c] -> (Either a b)
foldME' f (res@(Left _)) _ = res
foldME' f (res@(Right b)) (e:el) = foldME' f (f b e) el
foldME' f res [] = res

nothingOnLeft :: (Either a b) -> (Maybe b)
nothingOnLeft (Left _) = Nothing
nothingOnLeft (Right b) = Just $ b

leftOnNothing :: a -> (Maybe b) -> (Either a b)
leftOnNothing _ (Just b)  = return $ b
leftOnNothing a (Nothing) = Left(a)

flattenME :: a -> (Maybe (Either a b)) -> (Either a b)
flattenME _ (Just e) = e
flattenME a Nothing  = Left $ a

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

