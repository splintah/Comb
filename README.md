# Comb [![Build Status](https://travis-ci.org/splintah/Comb.svg?branch=master)](https://travis-ci.org/splintah/Comb)

Comb is a monadic parser combinator library for Haskell.
The monadic context (`m` in the definition of `Parser s m a`) is generic;
you can use a list, `Maybe`, or any other type that is an instance of `Monad` (and, for most primitive combinators, `MonadPlus`).
When using lists, the entire parse tree can be generated.

For example, when parsing many symbols `'a'` and then the token `"apple"`;
the parser would look something like:

```haskell
import Comb

aapple :: Parser Char Maybe (String, String)
aapple = do as    <- many (symbol 'a')
            apple <- token "apple"
            return (as, apple)
```

This parser is not greedy, as `parse aapple "aaapple"` returns `Just (("aa", "apple"), "")` (the second value in the tuple is the rest of the input), instead of failing because the first `'a'` of apple was already consumed.

## Usage

Add Comb as a dependency to your project.
Then put the following atop of the file using Comb.

```haskell
import Comb
```

## Documentation

Haddock documentation is available;
run `stack haddock` in this repository to create the mark-up.

## Examples

An example parser for arithmetic expressions can be found in `test/Arithmetic.hs`;
the parser supports the following operations:
* integers,
* grouped expressions, between parentheses (`(...)`),
* addition (`+`),
* subtraction (`-`),
* multiplication (`*`), and
* division (`/`).

The grammar for such expressions is:

```ebnf
Expr   ::= Term '+' Expr
         | Term '-' Expr
Term   ::= Factor '*' Term
         | Factor '/' Term
Factor ::= Integer
         | '(' Expr ')'

Integer ::= (Digit)+
Digit   ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
```

## TODO

The following are plans for features for this library;
their design is not finished yet:

* Add a way of error handling, preferably with a return type of `Either error (result, [symbol])`:
  - The type of the parser may become something like:
    ```haskell
    newtype Parser s e m a
      = Parser { parse :: ((a, [s]) -> m (a, [s])) -- ok function
                       -> (e -> m (a, [s]))        -- error function
                       -> e                        -- error/expected value
                       -> [s]                      -- input
                       -> m (a, [s])               -- output
               }
    ```
  - With the above type, the bound for `Monad m` can be dropped in most/all cases.
  - Add a function `(<?>)`, comparable to Parsec's, that adds/replaces an error message.
