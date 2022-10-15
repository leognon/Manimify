module Helper where

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (a,b) = (a, f b)
