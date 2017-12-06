module Parse where

    import Data.Char

    import Expression
    import ErrorHandler

    convertStringFloat :: String -> Float
    convertStringFloat [] = 0
    convertStringFloat (x:xs) = (fromIntegral (fromEnum x - 48)) + convertStringFloat xs * 10

  --  parseExp :: String -> Expression
  --  parseExp x = Variable x 
   -- parseExp x = Constant (convertStringFloat x)

    data Operator = AddOp | NotOp | MulOp
                    deriving (Show, Eq)

    data Token = TokOp Operator
              | TokVariable String
              | TokNum Float
              | TokEnd
        deriving (Show, Eq)
    
    tokenize :: String -> [Token]
    tokenize [] = []
    tokenize (c : cs) 
        | c == '+' = TokOp AddOp : tokenize cs
        | c == '-' = TokOp NotOp : tokenize cs
        | c == '*' = TokOp MulOp : tokenize cs
        | isDigit c = number c cs
        | isAlpha c = identifier c cs
        | isSpace c = tokenize cs
        | otherwise = error $ "Cannot tokenize " ++ [c]

    identifier :: Char -> String -> [Token]
    identifier c cs = let (name, cs') = span isAlphaNum cs in
                      TokVariable (c:name) : tokenize cs'

    number :: Char -> String -> [Token]
    number c cs = 
      let (digs, cs') = span isDigit cs in
      TokNum (read (c : digs)) : tokenize cs'

    ---- parser ----
    lookAhead :: [Token] -> Token
    lookAhead [] = TokEnd
    lookAhead (t:ts) = t

    accept :: [Token] -> [Token]
    accept [] = error "Nothing to accept"
    accept (t:ts) = ts

    expression :: [Token] -> (Expression, [Token])
    expression toks = 
      let (termTree, toks') = term toks
      in
          case lookAhead toks' of
            (TokOp AddOp) -> 
                let (exTree, toks'') = expression (accept toks') 
                in (Add termTree exTree, toks'')
            _ -> (termTree, toks')

    term :: [Token] -> (Expression, [Token])
    term toks = 
      let (facTree, toks') = factor toks
      in
          case lookAhead toks' of
            (TokOp MulOp)->
                let (termTree, toks'') = term (accept toks') 
                in (Mul facTree termTree, toks'')
            _ -> (facTree, toks')

    factor :: [Token] -> (Expression, [Token])
    factor toks = 
      case lookAhead toks of
          (TokNum x)     -> (Constant x, accept toks)
          (TokVariable str) -> (Variable str, accept toks)
          (TokOp NotOp)-> 
                let (facTree, toks') = factor (accept toks) 
                in (Not facTree, toks')
          _ -> error $ "Parse error on token: " ++ show toks

    parse :: [Token] -> Expression
    parse toks = let (exp, toks') = expression toks
                in
                  if null toks' 
                  then exp
                  else error $ "Leftover tokens: " ++ show toks'

    parseExpression :: String -> Expression
    parseExpression = parse . tokenize