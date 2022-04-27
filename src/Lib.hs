module Lib
( parseBody
, parseText
, parseParagraph
, Block(..)
, Parser
, regularParse
) where


import Text.Parsec (parse, try, Parsec)
import Text.Parsec.Combinator (many1, count, choice, optional, optionMaybe, notFollowedBy, lookAhead, eof, anyToken, manyTill)
import Text.Parsec.Char (satisfy)
import Text.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec.Prim (getState, setState)
import Text.Parsec.Char (letter, char, anyChar, noneOf)
import Control.Applicative ((<*), (<|>), many)
import Control.Monad (void, guard)
import Debug.Trace (trace)
import Text.Parsec (ParseError, oneOf)
import Text.Parsec.Prim (runP)
import Data.List (intersperse)
import Data.Char ( intToDigit )
import System.FilePath ( (</>) )
import System.Directory
import Data.Text (pack, Text)
import System.FilePath.Posix (takeDirectory)
import Data.Bits (Bits(xor))

data Block
  = Paragraph [Block]
  | Text String
  | Heading Int [Block]
  deriving (Show, Eq)

type Parser = Parsec String String

getParsers :: String -> [Parser Block]
getParsers state = [parseText]

parseText :: Parser Block
parseText = do
  line1 <- parseLine
  rest <- many parseNewLine
  return $ Text (concat $ line1:rest)

parseLine :: Parser String
parseLine = do
  void $ many (char ' ') -- Spaces at the beginning of a line are ignored
  first <- letter <|> char ' ' -- First char must be a letter or a character
  rest <- many (letter <|> char ' ')
  newline <- optionMaybe $ char '\n'
  return $ case newline of
    Just _ -> first:rest ++ " "
    Nothing -> first:rest

parseNewLine :: Parser String
parseNewLine = do
  hashes <- lookAhead (many (char '#'))
  if length hashes > 0 && length hashes <= 6
    then do
      next <- lookAhead anyToken
      guard (next /= ' ' && next /= '\n')
      parseLine
    else do
      parseLine

parseParagraph :: Parser Block
parseParagraph = do
  choice [void $ many (char '\n'), void $ eof]
  blocks <- many1 parseText
  choice [void $ many (char '\n'), void $ eof]
  return $ Paragraph blocks

parseHeading :: Parser Block
parseHeading = do
   hashes <- many1 (char '#')
   guard (length hashes <= 6)
   next <- anyToken
   guard (next == ' ' || next == '\n')
   void $ many (char ' ')
   str <- parseLine
   void $ many (char '\n')
--    choice [eof, void (char '\n')]
   return $ Heading (length hashes) [Text str]

parseBody :: Parser [Block]
parseBody = do
 paras <- many1 $ choice [try parseHeading, parseParagraph]
 return paras

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = runP p "" ""

------------------------------
tohtml [] = ""
tohtml ((Text a): xs) = a ++ tohtml xs
tohtml ((Paragraph b): xs) = "<p>" ++ tohtml b ++ "</p>" ++ tohtml xs
tohtml ((Heading a b): xs) = "<h" ++ [intToDigit a] ++ ">" ++ tohtml b ++ "</h" ++ [intToDigit a] ++ ">" ++ tohtml xs


gethtml :: Either a [Block] -> String
gethtml (Left a) = "None"
gethtml (Right a) = tohtml a



-------------------------------
headerText :: Block -> String
headerText (Text a) = a

readHeader :: [Block] -> String
readHeader [] = "None"
readHeader ((Heading a [b]): xs) = headerText b
readHeader (x: xs) = readHeader xs


getHeader :: Either a [Block] -> String
getHeader (Left a) = "None"
getHeader (Right a) = readHeader a

---------------------------------------------
mainheadername :: IO ()
mainheadername = do
  text <- readFile "test_markdown/test1.md"
  print $ getHeader $ regularParse parseBody text
-----------------------------------

main :: IO ()
main = do
  text <- readFile "test_markdown/test.md"
  print $ regularParse parseBody text

maintohtml :: IO ()
maintohtml = do
  text <- readFile "test_markdown/test.md"
  print $ gethtml $ regularParse parseBody text

mainwritehtml :: IO ()
mainwritehtml = do
  text <- readFile "test_markdown/test.md"
  writeFile "test_html/testhtml.html" $ gethtml $ regularParse parseBody text

--------------------------
getDirFiles inputDir =
  map (inputDir </>) <$> listDirectory inputDir


-- createHtmlFile text = gethtml $ regularParse parseBody text

createSidebar [] = "" 
createSidebar (x:xs) = "<a href=" ++ getPath "./" x ++ " class='w3-bar-item w3-button'>" ++  x ++ "</a>" ++ createSidebar xs

createIndexhtml a = do 
  let head = "<!DOCTYPE html><html> \
  \ <title>HaskyBook</title><meta name='viewport' content='width=device-width, initial-scale=1'> \
  \ <link rel='stylesheet' href='https://www.w3schools.com/w3css/4/w3.css'> \
  \ <body>"
  let sidebar_head = "<div class='w3-sidebar w3-light-grey w3-bar-block' style='width:100%'> \
  \ <h3 class='w3-bar-item'>Contents</h3>"
  let sidebar_tail = "</div>"
  head ++ sidebar_head ++ createSidebar a ++ sidebar_tail

createHtml a text = do
  let head = "<!DOCTYPE html><html> \
  \ <title>HaskyBook</title><meta name='viewport' content='width=device-width, initial-scale=1'> \
  \ <link rel='stylesheet' href='https://www.w3schools.com/w3css/4/w3.css'> \
  \ <body>"
  let sidebar_head = "<div class='w3-sidebar w3-light-grey w3-bar-block' style='width:25%'> \
  \ <h3 class='w3-bar-item'>Contents</h3>"
  let sidebar_tail = "</div>\
  \ <div style='margin-left:25%'>"
  let tail = "</div>"
  head ++ sidebar_head ++ createSidebar a ++ sidebar_tail ++ (gethtml $ regularParse parseBody text) ++ tail

getHeaderNames text = getHeader (regularParse parseBody text)

removeBlanks x = filter (\xs -> (xs /= ' ')) x

getPath a text = a ++ removeBlanks text ++ ".html"

readPath p a = p ++ a

getFilePathList contents = [getPath  "./docs/" "index"] ++ (map (getPath "./docs/") (map getHeaderNames contents))

getHtmlContentList contents = [createIndexhtml (map getHeaderNames contents)] ++ (map (createHtml (map getHeaderNames contents)) contents)

createDocFile path content= do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path content

createHtmlFiles = do
  let read_path = "./files/"
  files <- listDirectory read_path
  contents <- traverse readFile $ map (readPath read_path) files
  -- sequence $ zipWith writeFile (map createPath contents) (map createHtmlFile contents)
  sequence $ zipWith createDocFile  (getFilePathList contents) (getHtmlContentList contents)




