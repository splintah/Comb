module Main where

import Test.HUnit
import Arithmetic

main :: IO Counts
main = runTestTT $ TestList arithmeticTests
