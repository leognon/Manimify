module Types where

import Control.Applicative

inputHead :: Input -> InpChar
inputHead (Input []) = ('#', -1)
inputHead (Input (x:xs)) = x

-- To be an instance of Foldable, Input must be of kind (* -> *)
-- So there is a GenericInput type. But I only ever want to use
-- an input with (Char, Int) so only the Input constructor is used (along with the type synonym)
newtype GenericInput t = Input { getInp :: [t] }
    deriving Show
type InpChar = (Char, Int)
type Input = GenericInput InpChar

data Error = Error InpChar String
    deriving Show

instance Foldable GenericInput where
    foldMap f (Input s) = foldMap f s
    foldl f init (Input s) = foldl f init s
    null (Input s) = null s

newtype Parser a = Parser {
            runParser :: Input -> Either Error (a, Input)
                          }

instance Functor Parser where
    fmap f parser = Parser $ \inp -> do
        (val, rest) <- runParser parser inp
        return (f val, rest)

instance Applicative Parser where
    pure a = Parser $ \inp -> Right (a, inp)
    first <*> second = Parser $ \inp -> do
         (f, rest) <- runParser first inp
         runParser (f <$> second) rest
    first *> second = flip const <$> first <*> second
    first <* second = const <$> first <*> second

instance Alternative Parser where
    empty = Parser $ \inp -> Left $ Error (inputHead inp) "None Matched"
    first <|> second = Parser $ \inp ->
        case runParser first inp of
          Right (val, rest) -> Right (val, rest)
          Left _ -> case runParser second inp of
              Right (val, rest) -> Right (val, rest)
              Left _ -> runParser empty inp -- This ensures it obeys identity


data Expression = Add Expression Expression
                | Subtract Expression Expression
                | Mult Expression Expression
                | Divide Expression Expression
                | Nested Expression
                | Literal Int

instance Show Expression where
    show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Subtract a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
    show (Divide a b) = "(" ++ show a ++ " / " ++ show b ++ ")"
    show (Nested a) = "(" ++ show a ++ ")"
    show (Literal a) = show a


