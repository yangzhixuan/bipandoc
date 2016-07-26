import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Interpreter

import Abstract
import BX.BXHelpers
import qualified BX.MarkdownBX as BXM (markdownBX)
import BX.HTMLBX

import Parser.HTMLParser
import Parser.Markdown

import Text.Megaparsec


testHTML1 :: IO ()
testHTML1 = do
  putStrLn "test GetPut"
  i <- readFile "tests/1.html"
  putStrLn "original HTML document:\n==================="
  putStrLn i
  let htmlCST_= either (error. show) id (parse parseDoc "noname" i)
      htmlCST = refineDoc htmlCST_
      htmlAST = fromJust' (get htmlBX htmlCST)
  putStrLn "AST of HTML document:\n==================="
  putStrLn (show htmlAST)

  putStrLn "put AST back to CST and HTML file\n:=============="
  let htmlCST' = fromJust' (put htmlBX htmlCST htmlAST)
  putStrLn (prtDocument htmlCST')

  putStrLn "it html' equal to the input string?"
  putStrLn (show (prtDocument htmlCST' == i))

  putStrLn "put to an empty markdown file:\n==================="
  let mdCST = fromJust' (put BXM.markdownBX (MarkdownDoc []) htmlAST)
  putStrLn (printMarkdown mdCST)


fromJust' = maybe (error "nothing") id








---------------------------------------------------------
pHTML1 :: IO HTMLDoc
pHTML1 = do
  i <- IOS.readFile "1.html"
  let o  = either (error. show) id (parse parseDoc "1.html" i)
      oo = refineDoc o
  return oo

test1g :: IO ()
test1g = do
  cst <- pHTML1
  let ast = maybe (error "nothing") id (get htmlBX cst)
  putStrLn (show ast)

test1gTrace :: IO ()
test1gTrace = do
  i <- readFile "1.html"
  let o  = either (error. show) id (parse parseDoc "1.html" i)
      oo = refineDoc o
      ast = (getTrace htmlBX oo)
  putStrLn (show ast)


test1p :: IO ()
test1p = do
  cst <- pHTML1
  let ast = maybe (error "nothing") id (get htmlBX cst)
  let s' = maybe (error "nothing") id (put htmlBX cst ast)
  putStrLn (prtDocument s')

test1pModified :: IO ()
test1pModified = do
  cst <- pHTML1
  let newS = emptyHTML
      ast = maybe (error "nothing") id (get htmlBX cst)
      s'  = maybe (error "nothing") id (put htmlBX newS ast)
  putStrLn (prtDocument s')

test1pModifiedTrace' :: IO ()
test1pModifiedTrace' = do
  cst <- pHTML1
  let newS = emptyHTML
      ast = maybe (error "nothing") id (get htmlBX cst)
      s'  = maybe (error "nothing") id (put htmlBX newS ast)
  putStrLn (show s')


emptyHTML :: HTMLDoc
emptyHTML = HTMLDoc ""  doctype " " html "\n"
  where doctype = "<!DOCTYPE HTML>"
        html    = (GTree (CTag Block (Right "html") [] NormalClose)
                         [GTree (CTagText OtherText (Right "\n")) []
                         ,GTree (CTag Block (Right "head") [] NormalClose) [GTree (CTagText OtherText (Right "\n")) []]
                         ,GTree (CTagText OtherText (Right "\n  ")) []
                         ,GTree (CTag Block (Right "body") [] NormalClose) []])


getHTMLGTree :: IO (GTree CTag)
getHTMLGTree = do
  i <- IOS.readFile "1.html"
  let o  = either (error. show) id (parse parseDoc "1.html" i)
      oo = refineDoc o
      HTMLDoc _ _ _ (GTree (CTag _ _ _ _) html) _ = oo
      html' = filter (\x -> case x of GTree (CTag _ (Right "body") _ _) _ -> True; _ -> False) html
  return (head html')