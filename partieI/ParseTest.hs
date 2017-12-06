module ExpressionTest where

import Test.HUnit
import Parse

test1 :: Test
test1 = TestCase (do assertEqual "for eval (Store [(\"x\",2)]) (Mul (Variable \"x\") (Constant 2)) " (Just 2.0) (eval (Store [("x",1)]) (Mul (Variable "x") (Constant 2))))

tests :: Test
tests = TestList [TestLabel "test1" test1]

tests' :: Test
tests' = test [ "test1" ~: "eval (Store [(\"x\",2)) (Mul (Variable \"x\") (Constant 2))" ~: (Just 2.0) ~=? (eval (Store [("x",1)]) (Mul (Variable "x") (Constant 2)))]

main :: IO Counts
main = do _ <- runTestTT tests
          runTestTT tests'