module Helper where

import Types

makeInput :: String -> Input
makeInput s = Input (zip s [0..])

inputHead :: Input -> InpChar
inputHead (Input []) = ('#', -1)
inputHead (Input (x:xs)) = x

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (a,b) = (a, f b)
