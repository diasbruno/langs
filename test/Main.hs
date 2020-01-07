module Main where

import Control.Monad (void)
import JavascriptLexer
import JavascriptParser
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  let tests = testGroup "Javascript"
              [ lexerTests
              , parserTests
              ]
  in defaultMain tests
