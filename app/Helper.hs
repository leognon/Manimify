module Helper where

import Types

makeInput :: String -> Input
makeInput s = Input (zip s [0..])

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (a,b) = (a, f b)
