module Parsers where

import Data.Char
import Control.Applicative
import Helper
import Types


run :: String -> IO ()
run s = case runParser myParser (makeInput s) of
            Left (Error (char, pos) err) -> putStrLn $
                "Error: " ++ err ++ " on '" ++ [char] ++ "' at position " ++ show pos ++
                "\n" ++ prettyPrint (makeInput s) pos
            Right (x, rest) -> putStrLn $ "Parsed: " ++ show x ++ "\nAfter: " ++ prettyShowInp rest
        where
            prettyPrint :: Input -> Int -> String
            prettyPrint (Input inp) errPos = concat $ map (\(c, pos) -> if pos == errPos
                                                      then "\27[31m" ++ [c] ++ "\27[0m"
                                                      else [c]) inp

-- myParser :: Parser (Int, Int)
-- myParser = pairParser (numberParser <* stringParser ",") <*> numberParser

-- myParser :: Parser (Either () Int)
-- myParser = anyParser [Left <$> stringParser "hello", Right <$> numberParser]

-- myParser :: Parser [Int]
-- myParser = listParser numberParser

myParser = groupingParser

listParser :: String -> Parser a -> Parser [a]
listParser sep p = (:) <$> p <*> many (stringParser sep *> p)

pairParser :: Parser a -> Parser (b -> (a, b))
pairParser = (<$>) (,)

eitherParser :: Parser a -> Parser a -> Parser a
eitherParser = (<|>)

anyParser :: [Parser a] -> Parser a
anyParser = foldl (<|>) empty

groupingParser :: Parser GroupedInput
groupingParser = Parser $ \inp ->
    case runParser parseNext inp of
      Left e -> Left e
      Right (parsed, inp) ->
          if null inp
            then Right (parsed, inp)
            else Left $ Error (inputHead inp) "Unmatched ("
    where
        parseNext :: Parser GroupedInput
        parseNext = GroupedInput <$> many (parseChar <|> parseGroup)

        parseChar :: Parser GroupedInput
        parseChar = Parser $ \inp ->
            case inp of
              Input [] -> Left $ Error (inputHead inp) "End of inp"
              Input ((c, charNum):xs) ->
                  if c == '(' || c == ')'
                    then Left $ Error (inputHead inp) "Grouping char"
                    else Right (SingleInp (c, charNum), Input xs)
              -- TODO Also do { and maybe even quotes?
        parseGroup :: Parser GroupedInput
        parseGroup = stringParser "(" *> parseNext <* stringParser ")"


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
