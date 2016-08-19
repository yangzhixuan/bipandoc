{-# LANGUAGE OverloadedStrings #-}
module TestDocxReader where


import System.Console.ANSI
import Test.QuickCheck
import Data.DeriveTH
import Data.Eq

import Generics.BiGUL
import Data.List
import Data.Maybe
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH

import Abstract
import BX.MarkdownBX
import BX.HTMLBX
import CST.HTMLParser
import CST.HTMLParserDataType
import Reader.DocxReader
import CST.Markdown

--import Text.Show.Pretty

putStrInGreen :: String -> IO ()
putStrInGreen s = do
    setSGR [SetColor Foreground Vivid Green]
    putStr s
    setSGR [Reset]

testDocxReader filename = do

    cst <- getDocxCST filename
    putStrInGreen "Parsed DOCX: \n"
    putPretty cst
    let view = getDocxAST cst
    putStrLn ""
    --
    putStrInGreen "Got view: \n"
    putPretty view
    putStrLn ""
    --

    let (Just htmlCST) = put htmlBX emptyHTMLCST view
    putStrInGreen "Put to empty HTML, got: \n"
    putPretty htmlCST
    putStrLn ""

    putStrLn (prtDocument htmlCST)
    writeFile "docxToHtml.html" (prtDocument htmlCST)


    let (Just markdownCST) = put markdownBX (MarkdownDoc []) view
    putStrInGreen "Put to empty Markdown, got: \n"
    putPretty markdownCST
    putStrLn ""

    putStrLn (printMarkdown markdownCST)
    writeFile "docxToMarkdown.md" (printMarkdown markdownCST)

    --let htmlCST' = fmap (parseHTML . prtDocument) htmlCST
    --putStrInGreen "Print and re-parse htmlCST: \n"
    --putPretty htmlCST'
    --putStrLn ""
    ----
    --let view' = htmlCST' >>= get htmlBX
    --putStrInGreen "Got view from HTML: \n"
    --putPretty view'
    --putStrLn ""

    --
    --let src' = (view' >>= \v -> put markdownBX md v)
    --putStrInGreen "Put back to source md:\n"
    --putPretty src'
    --putStrLn ""
    --putStrInGreen "Equal to the original source? "
    --setSGR [SetColor Foreground Vivid Red]
    --print (fromMaybe False (src' >>= \s -> return (s == md)))
    --setSGR [Reset]
    --putStrLn ""
    ----
    --putStrInGreen "Printed put-back source equals orignal file? "
    --setSGR [SetColor Foreground Vivid Red]
    --let f' = fmap printMarkdown src'
    --if (Just f == f')
    --   then putStrLn "True"
    --   else do putStrLn "False"; putStrLn (f ++ "\nvs.\n\n" ++ (fromMaybe "" f'))
    putStrLn ""
    setSGR [Reset]
