module Parser where

-- Id: 1
-- Maybe: fail or 1
-- List: fail or 1 or more
-- NEL: 1 or more

type List a = [a]

class Input a where
  isEmpty :: a -> Bool

data Parser a b = MkParser {runParser :: a -> List (a, b)}

parse :: (Input a) => Parser a b -> a -> Either String b
parse p a = case runParser p a of
  [] -> Left "Parse Error"
  [(body, res)] ->
    if isEmpty body
    then Left "Incomplete parse"
    else Right res
  [x:xs] -> Left "Parser Error: Multiple Parse Trees"
