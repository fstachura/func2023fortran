module Map (
    SimpleMap,
    mapLookup,
    mapInsert,
    mapDelete,
    simpleMap
) where

type SimpleMap k v = [(k,v)]

mapLookup :: Ord k => k -> SimpleMap k v -> Maybe v
mapLookup = lookup 

mapInsert :: Ord k => k -> v -> SimpleMap k v -> SimpleMap k v
mapInsert k v m = (k,v):(mapDelete k m)

mapDelete :: Ord k => k -> SimpleMap k v -> SimpleMap k v
mapDelete k = filter (((/=) k) . fst)

simpleMap = []
