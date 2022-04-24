module Main where

import Lib (parseBody, Parser, regularParse, Block (Heading))


main :: IO ()
main = do
  text <- readFile "test.md"
  print $ regularParse parseBody text