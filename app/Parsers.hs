module Parsers where

import Data.Char
import Helper
import Types


makeInput :: String -> Input
makeInput s = Input (zip s [0..])

inputHead :: Input -> InpChar
inputHead (Input []) = ('#', -1)
inputHead (Input (x:xs)) = x

run :: String -> IO ()
run s = case runParser numberParser (makeInput s) of
            Left (Error charNum err) -> putStrLn $ "Error on " ++ show charNum ++ " with " ++ err
            Right (parsed, rest) -> putStrLn $ "Parsed " ++ show (parsed, rest)

numberParser :: Parser Int
numberParser = Parser
    (\inp ->
        if not (null inp) && isDigit (fst $ inputHead inp)
            then Right $ parseNum 0 inp
            else Left $ Error (inputHead inp) $ "Not a num: " ++ take 5 (map fst $ getInp inp) ++ "...")
    where
        parseNum :: Int -> Input -> (Int, Input)
        parseNum num (Input []) = (num, Input [])
        parseNum num (Input ((x,charNum):xs)) = if isDigit x
                                 then parseNum (10*num + digitToInt x) (Input xs)
                                 else (num, Input ((x,charNum) : xs))
