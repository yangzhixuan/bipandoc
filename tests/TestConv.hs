import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Interpreter

import Abstract
import BX.BXHelpers
import qualified BX.MarkdownBX as BXM (markdownBX)
import BX.HTMLBX

import CST.HTMLParser
import CST.HTMLParserDataType
import CST.Markdown

import Text.Megaparsec

import Control.Monad.Writer

import Debug.Trace

testGetPut1 :: IO ()
testGetPut1 = do
  putStrLn "test GetPut"
  putStrLn ""
  i <- readFile "test111.html"
  putStrLn "original HTML document:\n==================="
  putStrLn i
  let htmlCST= parseHTML i
      htmlAST = maybe (error "parse cst to ast error") id (get htmlBX htmlCST)
  putStrLn "CST of the HTML document"
  putStrLn ""
  putStrLn (ppShow htmlCST)
  putStrLn "AST of the HTML document:\n==================="
  putStrLn (ppShow htmlAST)
  putStrLn ""
  putStrLn "put AST back to CST and HTML file\n:=============="
  let htmlCST' = maybe (error "print ast to cst error") id (put htmlBX htmlCST htmlAST)
  putStrLn (prtDocument htmlCST')
  putStrLn ""
  putStrLn "is html' equal to the input string?"
  putStrLn (ppShow (prtDocument htmlCST' == i))
  putStrLn ""
  putStrLn "put to an empty markdown file:\n==================="
  let mdCST = maybe (error "print ast to cst error") id (put BXM.markdownBX (MarkdownDoc []) htmlAST)
  let md    = (printMarkdown mdCST)
  putStrLn md
  putStrLn ""
  putStrLn "parse that generated markdown file to a new markdown CST:\n==================="
  putStrLn ""
  let mdCST' = parseMarkdown md
  putStrLn (ppShow mdCST')
  putStrLn "parse that generated markdown CST' to AST':\n==================="
  putStrLn ""
  let ast' = maybe (error "parse cst to ast error") id (get BXM.markdownBX mdCST')
  putStrLn (ppShow ast')

  putStrLn "put that markdown AST back to the html CST':\n==================="
  putStrLn ""
  --putStrLn (show $ putTrace htmlBX htmlCST ast')
  let htmlCST' = maybe (error "print ast' back to cst' error") id (put htmlBX htmlCST ast')
  putStrLn (ppShow htmlCST')

  putStrLn "newly generated html document:"
  putStrLn ""
  putStrLn (prtDocument htmlCST')
  putStrLn ""
  putStrLn "is the newly generated html document equl to the origin one?:\n==================="
  putStrLn (ppShow (prtDocument htmlCST' == i))


testPutToAnEmptyHTML = test1pModified


---------------------------------------------------------
pHTML1 :: IO HTMLDoc
pHTML1 = do
  i <- readFile "test111.html"
  let o  = parseHTML i
  return o

test1g :: IO ()
test1g = do
  cst <- pHTML1
  let ast = maybe (error "nothing") id (get htmlBX cst)
  putStrLn (ppShow ast)

test1gTrace :: IO ()
test1gTrace = do
  i <- readFile "test111.html"
  let o  = parseHTML i
      ast = (getTrace htmlBX o)
  putStrLn (ppShow ast)


test1p :: IO ()
test1p = do
  cst <- pHTML1
  let ast = maybe (error "parse cst to ast error") id (get htmlBX cst)
  let s' = maybe (error "print ast to cst error") id (put htmlBX cst ast)
  putStrLn (prtDocument s')

test1pModified :: IO ()
test1pModified = do
  cst <- pHTML1
  let newS = emptyHTMLCST
      ast = maybe (error "parse cst to ast error") id (get htmlBX cst)
      s'  = maybe (error "print ast to cst error") id (put htmlBX newS ast)
  putStrLn (prtDocument s')

test1pModifiedTrace :: IO ()
test1pModifiedTrace = do
  cst <- pHTML1
  let newS = emptyHTMLCST
      ast = maybe (error "parse cst to ast error") id (get htmlBX cst)
      s'  = putTrace htmlBX newS ast
  putStrLn (ppShow s')
