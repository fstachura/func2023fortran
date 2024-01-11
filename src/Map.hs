module Map (
) where

type SimpleMap k v = [(k,v)]

lookup :: Ord k => k -> SimpleMap k v -> Maybe v
lookup k m = case filter (((==) k) . fst) m of
                (x:xs) -> Just (snd x)
                ([]) -> Nothing

insert :: Ord k => k -> v -> SimpleMap k v -> SimpleMap k v
insert k v m = (k,v):(delete k m)

delete :: Ord k => k -> SimpleMap k v -> SimpleMap k v
delete k = filter (((/=) k) . fst)
