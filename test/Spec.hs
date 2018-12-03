import Parser
import Test.HUnit
import Arithmetic

main :: IO Counts
main = runTestTT $ TestList $ concat
  [ exprTests
  , exprIntTests
  ]

exprTests = [ TestLabel "associativity addition" assocAdd
            , TestLabel "associativity subtraction" assocSub
            , TestLabel "associativity multiplication" assocMul
            , TestLabel "associativity division" assocDiv
            , TestLabel "precedence" precedence
            ]
  where
    assocAdd = TestCase $
      assertEqual "1+2+3 == (1 :+: 2) :+: 3"
        ((Con 1 :+: Con 2) :+: Con 3)
        (parseExpr "1+2+3")
    assocSub = TestCase $
      assertEqual "1-2-3 == (1 :-: 2) :-: 3"
        ((Con 1 :-: Con 2) :-: Con 3)
        (parseExpr "1-2-3")
    assocMul = TestCase $
      assertEqual "1*2*3 == (1 :*: 2) :*: 3"
        ((Con 1 :*: Con 2) :*: Con 3)
        (parseExpr "1*2*3")
    assocDiv = TestCase $
      assertEqual "1/2/3 == (1 :/: 2) :/: 3"
        ((Con 1 :/: Con 2) :/: Con 3)
        (parseExpr "1/2/3")
    precedence = TestCase $
      assertEqual "1+2*3-6/3-1 == ((1 :+: (2 :*: 3)) :-: (6 :/: 3)) :-: 1"
        (((Con 1 :+: (Con 2 :*: Con 3)) :-: (Con 6 :/: Con 3)) :-: Con 1)
        (parseExpr "1+2*3-6/3-1")

exprIntTests = [ TestLabel "addition" add
               , TestLabel "subtraction" sub
               , TestLabel "multiplication" mul
               , TestLabel "division" div
               , TestLabel "parens" parens
               ]
  where
    add = TestCase $
      assertEqual "1+2+3 == 6"
        6
        (evalExpr "1+2+3")
    sub = TestCase $
      assertEqual "3-2-1 == 0"
        0
        (evalExpr "3-2-1")
    mul = TestCase $
      assertEqual "1*2*3 == 6"
        6
        (evalExpr "1*2*3")
    div = TestCase $
      assertEqual "12/2/3 == 2"
        2
        (evalExpr "12/2/3")
    parens = TestCase $
      assertEqual "(1+2)*3 == 9"
        9
        (evalExpr "(1+2)*3")
