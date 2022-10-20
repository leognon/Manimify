module Parsers where

import Data.Char
import Helper
import Types


run :: String -> IO ()
run s = case runParser (parseSequential (stringParser "asdf") numberParser) (makeInput s) of
            Left (Error charNum err) -> putStrLn $ "Error on " ++ show charNum ++ " with " ++ err
            Right (x, rest) -> putStrLn $ "Parsed " ++ show x ++ "\n" ++ show rest

parseSequential :: Parser a -> Parser b -> Parser b
parseSequential parserA parserB = Parser $ \inp ->
    case runParser parserA inp of
      Left e -> Left e
      Right (_, rest) -> case runParser parserB rest of
                    Left e -> Left e
                    Right (val', rest') -> Right (val', rest')

parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither parserA parserB = Parser $ \inp ->
    case runParser parserA inp of
      Right (val, rest) -> Right (Left val, rest)
      Left err -> case runParser parserB inp of
                    Right (val', rest') -> Right (Right val', rest')
                    Left err -> Left err -- TODO This should probably have a custom messaged, not just the last one attempted



stringParser :: String -> Parser ()
stringParser s = Parser $ parseStr s
    where
        parseStr :: String -> Input -> Either Error ((), Input)
        parseStr [] y = Right ((), y)
        parseStr (x:xs) (Input []) = Left $ Error ('#', -1) "Unexpected end of input"
        parseStr (x:xs) (Input ((c,charNum):ys))
          | x == c = parseStr xs (Input ys)
          | otherwise = Left $ Error (c, charNum) "Char mismatch"

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
