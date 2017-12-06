module Parse where

    import Data.Char

    import Expression
    import ErrorHandler
    --http://book.realworldhaskell.org/read/using-parsec.html

    

    convertStringFloat :: String -> Float
    convertStringFloat [] = 0
    convertStringFloat (x:xs) = (fromIntegral (fromEnum x - 48)) + convertStringFloat xs * 10

--    product3 :: [Int] -> Int
 --   product3 [] = 1
  --  product3 (n:ns) = n * product3 ns

    parseExp :: String -> Expression
  --  parseExp x = Variable x 
    parseExp x = Constant (convertStringFloat x)

 
    
    parseExpression :: String -> Maybe Expression
    parseExpression str = case (unsafeCleanup $ parseExp str) of
                                              Nothing -> Nothing 
                                              Just result -> Just result
