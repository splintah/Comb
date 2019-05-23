{-|
Module      : Test
Description : Test suite
Copyright   : (C) Splinter Suidman, 2019
License     : AGPL-3
-}
module Main where

import Test.HUnit
import Arithmetic

main :: IO Counts
main = runTestTT $ TestList arithmeticTests
