module Lib
( parseBody
, parseText
, parseParagraph
, Block(..)
, Parser
, regularParse
, getFilePathList
, getHtmlContentList
, createDocFile
, readPath
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
  | Italic [Block]
  | Bold [Block]
  deriving (Show, Eq)

type Parser = Parsec String String

getParsers :: String -> [Parser Block]
getParsers state = [parseText]

parseText :: Parser Block
parseText = do
  line1 <- parseLine
  rest <- many parseNewLine
  return $ Text (unwords $ line1:rest)

parseLine :: Parser String
parseLine = do
  void $ many (char ' ') -- Spaces at the beginning of a line are ignored
  line <- many1 (noneOf (['\n', '*'])) -- First char must be a letter or a character
  newline <- optionMaybe $ char '\n'
  return $ case newline of
    Just _ -> line ++ ""
    Nothing -> line

parseItalic = between' delimiter delimiter parseText >>= return . Italic
  where delimiter = count 1 $ char '*'


parseBold = between' delimiter delimiter parseText >>= return . Bold
  where delimiter = count 2 $ char '*'

between' a b c = a *> manyTill c (try b)

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
  blocks <- many1 $ choice [try parseBold, parseItalic, parseText]
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
tohtml ((Italic b): xs) = "<em>" ++ tohtml b ++ "</em>" ++ tohtml xs
tohtml ((Bold b): xs) = "<strong>" ++ tohtml b ++ "</strong>" ++ tohtml xs
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
  text <- readFile "files/test.md"
  print $ getHeader $ regularParse parseBody text
-----------------------------------

main :: IO ()
main = do
  text <- readFile "files/test.md"
  print $ regularParse parseBody text

maintohtml :: IO ()
maintohtml = do
  text <- readFile "files/test.md"
  print $ gethtml $ regularParse parseBody text

mainwritehtml :: IO ()
mainwritehtml = do
  text <- readFile "files/test.md"
  writeFile "test_html/testhtml.html" $ gethtml $ regularParse parseBody text

--------------------------
getDirFiles inputDir =
  map (inputDir </>) <$> listDirectory inputDir


-- createHtmlFile text = gethtml $ regularParse parseBody text

createSidebar [] = "" 
createSidebar (x:xs) = "<div class = 'active'> <a href=" ++ getPath "./" x ++ ">" ++  x ++ "</a></div>" ++ createSidebar xs

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
  let head = "<!DOCTYPE html><html>  <title>HaskyBook</title><meta name='viewport' content='width=device-width, initial-scale=1'> \
 \ <link href='https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css' rel='stylesheet' integrity='sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3' crossorigin='anonymous'> \
\ <script src='https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js' integrity='sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p' crossorigin='anonymous'></script> \
 \ <style> \
\ @import 'https://fonts.googleapis.com/css?family=Poppins:300,400,500,600,700'; \ 
\ body { \ 
\    font-family: 'Poppins', sans-serif; \
\    background: #fafafa; \
\} \
\p { \
\    font-family: 'Poppins', sans-serif;\
\   font-size: 1.1em;\
\   font-weight: 300;\
\   line-height: 1.7em;\
\   color: #999;\
\}\

\a,\
\a:hover,\
\a:focus {\
\    color: inherit;\
\   text-decoration: none;\
\   transition: all 0.3s;\
\}\


\/* ---------------------------------------------------\
  \  SIDEBAR STYLE\
\----------------------------------------------------- */\

\.wrapper {\
 \   display: flex;\
  \  width: 100%;\
   \ align-items: stretch;\
   \ height: 100%;\
    
\}\

\#sidebar {\
    \min-width: 250px;\
    \max-width: 250px;\
    \background: #7386D5;\
    \color: #fff;\
    \transition: all 0.3s;\
    \align-items: stretch;\
\}\

\#sidebar.active {\
  \  margin-left: -250px;\
   \ padding: 20px;\
 \   height: 100%;\
\}\

\#sidebar .sidebar-header {\
\    padding: 20px;\
 \   background: #6d7fcc;\
\}\

\#sidebar a {\
    \padding: 10px;\
    \font-size: 1.1em;\
    \display: block;\
\}\

\#sidebar a:hover {\
    \color: #7386D5;\
    \background: #fff;\
    \padding: 20px;\
\}\

\.content {\
    \width: 100%;\
    \padding: 50px;\
    \min-height: 100vh;\
    \transition: all 0.3s;\
    \margin-left:3%;\
    \margin-top:5%;\
\}\

\</style> \
\<body>"
  let sidebar_head = "<div class='wrapper'>\
\<nav id='sidebar'>\
 \<h3 class='sidebar-header'>Contents</h3>"
  let sidebar_tail = "</nav>\
 \<div class='content'>"
  let tail = "</div></div>"
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




