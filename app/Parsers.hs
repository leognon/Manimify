module Parsers where

import Data.Char
import Control.Applicative
import Helper
import Types


-- myParser :: Parser (Int, Int)
-- myParser = pairParser (numberParser <* stringParser ",") <*> numberParser
myParser = eitherParser (stringParser "hello") (stringParser "asdf")

run :: String -> IO ()
run s = case runParser myParser (makeInput s) of
            Left (Error charNum err) -> putStrLn $ "Error on " ++ show charNum ++ " with " ++ err
            Right (x, rest) -> putStrLn $ "Parsed " ++ show x ++ "\n" ++ show rest

pairParser :: Parser a -> Parser (b -> (a, b))
pairParser = (<$>) (,)

eitherParser :: Parser a -> Parser a -> Parser a
eitherParser = (<|>)

stringParser :: String -> Parser ()
stringParser s = Parser $ parseStr s
    where
        parseStr :: String -> Input -> Either Error ((), Input)
        parseStr [] y = Right ((), y)
        parseStr (x:xs) (Input []) = Left $ Error ('#', -1) "Unexpected end of input"
        parseStr (x:xs) (Input ((c,charNum):ys))
          | x == c = parseStr xs (Input ys)
          | otherwise = Left $ Error (c, charNum) ("Char mismatch. Expected " ++ [x])

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
