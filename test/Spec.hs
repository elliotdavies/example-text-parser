module Main where

import Test.HUnit
import Lib (parse)

main :: IO ()
main = do
  runTestTT parserTests
  return ()

-- Test the lexer
parserTests = TestLabel "parse" $ test [

  "finds matches in a string" ~: do
    let searchText = "hello"
    let searchSpace = "something hello world"
    let expected = Right "something <mark>hello</mark> world"
    assertEqual "1" expected (parse searchText searchSpace)
  ,

  "finds matches in a string with punctuation" ~: do
    let searchText = "hi i'm Elliot"
    let searchSpace = "Hello, Hi, I'm Elliot"
    let expected = Right "Hello, <mark>Hi, I'm Elliot</mark>"
    assertEqual "1" expected (parse searchText searchSpace) ]
