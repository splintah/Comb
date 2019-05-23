module Arithmetic
  ( arithmeticTests
  ) where

import           Comb       hiding (Parser)
import qualified Comb       as Comb
import           Test.HUnit

arithmeticTests =
  [ TestLabel "parse addition" $ TestCase $ assertEqual
      "\"1 + 2 + 3\" -> (1 :+: 2) :+: 3"
      (parse expr1 "1 + 2 + 3")
      (Just ((Con 1 :+: Con 2) :+: Con 3))
  , TestLabel "parse subtraction" $ TestCase $ assertEqual
      "\"1 - 2 - 3\" -> (1 :-: 2) :-: 3"
      (parse expr1 "1 - 2 - 3")
      (Just ((Con 1 :-: Con 2) :-: Con 3))
  , TestLabel "parse multiplication" $ TestCase $ assertEqual
      "\"1 * 2 * 3\" -> (1 :*: 2) :*: 3"
      (parse expr1 "1 * 2 * 3")
      (Just ((Con 1 :*: Con 2) :*: Con 3))
  , TestLabel "parse division" $ TestCase $ assertEqual
      "\"1 / 2 / 3\" -> (1 :/: 2) :/: 3"
      (parse expr1 "1 / 2 / 3")
      (Just ((Con 1 :/: Con 2) :/: Con 3))
  , TestLabel "parse exponentiation" $ TestCase $ assertEqual
      "\"1 ^ 2 ^ 3\" -> 1 :^: (2 :^: 3)"
      (parse expr1 "1 ^ 2 ^ 3")
      (Just (Con 1 :^: (Con 2 :^: Con 3)))
  , TestLabel "operator precedences" $ TestCase $ assertEqual
      "\"1 + 2 * 3 ^ 4 / 5 - 6\" -> ((1 :+: ((2 :*: (3 :^: 4)) :/: 5)) :-: 6)"
      (parse expr1 "1 + 2 * 3 ^ 4 / 5 - 6")
      (Just ((Con 1 :+: ((Con 2 :*: (Con 3 :^: Con 4)) :/: Con 5)) :-: Con 6))
  ]

type Parser a = Comb.Parser Maybe String a

parse :: (Functor f) => Comb.Parser f s a -> s -> f a
parse p s = fst <$> runParser p s

infixl 6 :+:, :-:
infixl 7 :*:, :/:
infixr 8 :^:
data Expr
  = Con Int
  | Expr :+: Expr
  | Expr :-: Expr
  | Expr :*: Expr
  | Expr :/: Expr
  | Expr :^: Expr
  deriving (Show, Eq)

-- Note: the type of the parsers below is not explicitly typed out, to show the
-- power of type inference for parser combinators.
expr1 =
  chainl expr2
    (  (:+:) <$ symbol '+'
   <|> (:-:) <$ symbol '-')
expr2 =
  chainl expr3
    (  (:*:) <$ symbol '*'
   <|> (:/:) <$ symbol '/')
expr3 = chainr expr4 ((:^:) <$ symbol '^')
expr4 = many space *> (parenthesised expr1 <|> Con <$> int) <* many space

foldExpr ::
     (Int -> e)    -- Con
  -> (e -> e -> e) -- :+:
  -> (e -> e -> e) -- :-:
  -> (e -> e -> e) -- :*:
  -> (e -> e -> e) -- :/:
  -> (e -> e -> e) -- :^:
  -> Expr -> e
foldExpr con add sub mul div pow = fold
  where
    fold e = case e of
      Con i   -> con i
      a :+: b -> add (fold a) (fold b)
      a :-: b -> sub (fold a) (fold b)
      a :*: b -> mul (fold a) (fold b)
      a :/: b -> div (fold a) (fold b)
      a :^: b -> pow (fold a) (fold b)

exprEval :: Expr -> Int
exprEval = foldExpr id (+) (-) (*) div (^)
