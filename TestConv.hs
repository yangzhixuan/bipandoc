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