module Parsers where

import Data.Char
import Control.Applicative
import Helper
import Types


-- myParser :: Parser (Int, Int)
-- myParser = pairParser (literalParser <* stringParser ",") <*> literalParser
-- myParser = eitherParser (stringParser "hello") (stringParser "asdf")
-- myParser = expressionParser -- splitParser '+'
myParser = fullParser expressionParser

run :: String -> IO ()
run s = case runParser myParser (makeInput s) of
            Left (Error charNum err) -> putStrLn $ "Error on " ++ show charNum ++ " with " ++ err
            Right (x, rest) -> putStrLn $ "Parsed " ++ show x ++ "\n" ++ show rest

pairParser :: Parser a -> Parser (b -> (a, b))
pairParser = fmap (,)

eitherParser :: Parser a -> Parser a -> Parser a
eitherParser = (<|>)

-- Requires the entire string be parsed
fullParser :: Parser a -> Parser a
fullParser p = p <* nothingParser
    where
        nothingParser :: Parser ()
        nothingParser = Parser $ \inp ->
            if null inp then Right ((), Input [])
                        else Left $ Error (inputHead inp) "Did not fully parse"

-- (1+2)*3

-- expr = term (PLUS expr) | term (MINUS expr) | term
-- term = lit (TIMES term) | lit (DIVIDE term) | ( expr ) | lit
-- lit = INTEGER | ( expr )

{-
expr: 1 * 2 + 3
    term: 1 * 2 + 3
        lit: 1
-}

{-
expression
    ::= term ((PLUS|MINUS) term)
term
    ::= factor ((FSLASH|ASTERISK) factor)
-}

fixParens :: Expression -> Expression
fixParens (Subtract a (Add b c)) = Add (Subtract a b) c
fixParens (Divide a (Mult b c)) = Mult (Divide a b) c
fixParens x = x

expressionParser :: Parser Expression
expressionParser = fixParens <$> (add <|> sub <|> termParser)
    where
        add = Add <$> termParser <*> (stringParser "+" *> expressionParser)
        sub = Subtract <$> termParser <*> (stringParser "-" *> expressionParser)


termParser :: Parser Expression
termParser = fixParens <$> mult <|> div <|> parens <|> litParser
    where
        mult = Mult <$> litParser <*> (stringParser "*" *> termParser)
        div = Divide <$> litParser <*> (stringParser "/" *> termParser)
        parens = Nested <$> (stringParser "(" *> expressionParser <* stringParser ")")


litParser =
    literalParser <|>
    stringParser "(" *> expressionParser <* stringParser ")"

stringParser :: String -> Parser Input
stringParser s = Parser $ parseStr s
    where
        parseStr :: String -> Input -> Either Error (Input, Input)
        parseStr [] y = Right (Input [], y)
        parseStr (x:xs) (Input []) = Left $ Error ('#', -1) "Unexpected end of input"
        parseStr (x:xs) (Input ((c,pos):ys))
          | x == c = case parseStr xs (Input ys) of
                       Left e -> Left e
                       Right (Input str, rest) -> Right (Input $ (c,pos) : str, rest)
          | otherwise = Left $ Error (c, pos) ("Char mismatch. Expected " ++ [x])

literalParser :: Parser Expression
literalParser = Literal <$> Parser
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
