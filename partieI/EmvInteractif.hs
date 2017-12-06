module EmvInteractif where

    import Expression
    import Parse

 --    :: String -> Expression -> Maybe Float 
  --  eval (Store store) str = case (unsafeCleanup $ evaExp (Store store) str) of
    --                                             Nothing -> Nothing
      --                                           result -> result     

    main :: IO()
    main = 
        putStr "Rentre une expression (en Expression) ou une commande: ">>
        getLine >>= \xs ->
        putStr (show (parseExpression xs)) >>
     --   putStr (show (test xs)) >>
        putStrLn ""

