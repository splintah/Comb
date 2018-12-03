# Comb [![Build Status](https://travis-ci.org/splintah/Comb.svg?branch=master)](https://travis-ci.org/splintah/Comb)

Comb is a monadic parser combinator library for Haskell.
Lists are used for monadic context, which allows getting the entire parse tree (and not just the greedy left-most derivation).

For example, when parsing many symbols `'a'` and then the token `"apple"`;
the parser would look something like:

```haskell
import Comb

aapple :: Parser Char (String, String)
aapple = do as    <- many (symbol 'a')
            apple <- token "apple"
            return (as, apple)
```

Because of the usage of lists, this parser is not greedy, and `parse aapple "aaapple"` returns `("aa", "apple")`, instead of failing because the first `'a'` of apple was already consumed.

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
* grouped expressions, between parentheses (`(...)`),
* integers,
* addition (`+`),
* subtraction (`-`),
* multiplication (`*`), and
* division (`/`).

The grammar for such an expressions is:

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
