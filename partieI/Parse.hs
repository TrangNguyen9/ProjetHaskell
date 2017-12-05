module Parse where

    import Expression
    import ErrorHandler
    --http://book.realworldhaskell.org/read/using-parsec.html
     parseExp :: String -> Expression
    parseExp = parse . tokenize
    
    parseExpression :: String -> Maybe Expression
    parseExpression str = case (unsafeCleanup $ parseExp str) of
                                                Nothing -> Nothing 
                                                Just result -> Just result

