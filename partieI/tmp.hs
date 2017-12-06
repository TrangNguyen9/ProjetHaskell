import Expression

import Control.Monad
import Text.Parsec
import Control.Applicative hiding ((<|>))

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

number = many1 digit

plus = char '+' *> number

minus = char '-' <:> number

integer = plus <|> minus <|> number

float = fmap rd $ integer <++> decimal <++> exponent
    where rd       = read :: String -> Float
          decimal  = option "" $ char '.' <:> number
          exponent = option "" $ oneOf "eE" <:> integer

main = forever $ do putStrLn "Enter a float: "
                    input <- getLine
                    parseTest float input