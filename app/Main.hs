module Main where

import Lib (parseBody, Parser, regularParse, Block (Heading),getFilePathList,getHtmlContentList,createDocFile,readPath)
import System.Directory


main :: IO [()]
main = do
  let read_path = "./files/"
  files <- listDirectory read_path
  contents <- traverse readFile $ map (readPath read_path) files
  sequence $ zipWith createDocFile  (getFilePathList contents) (getHtmlContentList contents)