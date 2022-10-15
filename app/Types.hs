module Types where


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
-- TODO Now that GenericInput is foldable, should I create a definition for foldl
-- and use that everywhere to nicely handle getting the chars without messing with
-- the positions?

newtype Parser a = Parser {
            runParser :: Input -> Either Error (a, Input)
                          }
