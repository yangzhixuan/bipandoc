{-# LANGUAGE OverloadedStrings #-}
module TestBX where


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
import Parser.Markdown
import Parser.HTMLParser
import BX.MarkdownBX
import BX.HTMLBX

putStrInGreen :: String -> IO ()
putStrInGreen s = do
    setSGR [SetColor Foreground Vivid Green]
    putStr s
    setSGR [Reset]

testBX filename = do
    f <- readFile filename
    let md = parseMarkdown  f
    putStrInGreen "Parsed markdown: \n"
    putPretty md
    let view = get markdownBX md
    putStrLn ""
    --
    putStrInGreen "Got view: \n"
    putPretty view
    putStrLn ""
    --
    let htmlCST = view >>= put htmlBX emptyHTMLCST
    putStrInGreen "Put to empty HTML, got: \n"
    putPretty htmlCST
    putStrLn ""

    let htmlCST' = fmap (parseHTML . prtDocument) htmlCST
    putStrInGreen "Print and re-parse htmlCST: \n"
    putPretty htmlCST'
    putStrLn ""
    --
    let view' = htmlCST' >>= get htmlBX
    putStrInGreen "Got view from HTML: \n"
    putPretty view'
    putStrLn ""
    --
    let src' = (view' >>= \v -> put markdownBX md v)
    putStrInGreen "Put back to source md:\n"
    putPretty src'
    putStrLn ""
    putStrInGreen "Equal to the original source? "
    setSGR [SetColor Foreground Vivid Red]
    print (fromMaybe False (src' >>= \s -> return (s == md)))
    setSGR [Reset]
    putStrLn ""
    --
    putStrInGreen "Printed put-back source equals orignal file? "
    setSGR [SetColor Foreground Vivid Red]
    let f' = fmap printMarkdown src'
    if (Just f == f')
       then putStrLn "True"
       else do putStrLn "False"; putStrLn (f ++ "\nvs.\n\n" ++ (fromMaybe "" f'))
    putStrLn ""
    setSGR [Reset]
