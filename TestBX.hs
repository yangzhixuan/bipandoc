{-# LANGUAGE OverloadedStrings #-}
module TestBX where


import System.Console.ANSI
import Test.QuickCheck
import Data.DeriveTH
import Data.Eq

import Generics.BiGUL
import Data.List
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH

import Abstract
import Markdown
import BX

putStrInGreen :: String -> IO ()
putStrInGreen s = do
    setSGR [SetColor Foreground Vivid Green]
    putStr s
    setSGR [Reset]

test filename = do
    f <- readFile filename
    let md = parseMarkdown  f
    putStrInGreen "Parsed markdown: \n"
    print md
    let view = get markdownBX md
    putStrLn ""
    --
    putStrInGreen "Got view: \n"
    print view
    putStrLn ""
    --
    let src' = (view >>= \v -> put markdownBX md v)
    putStrInGreen "Put back source:\n"
    print src'
    putStrInGreen "Equal to orignal source? "
    setSGR [SetColor Foreground Vivid Red]
    print (src' >>= \s -> return (s == md))
    setSGR [Reset]
    putStrLn ""
    --
    putStrInGreen "Printed put-back source equals orignal file? "
    setSGR [SetColor Foreground Vivid Red]
    let f' = fmap printMarkdown src'
    if (Just f == f')
       then putStrLn "True"
       else do putStrLn "False"; putStrLn (f ++ "\nvs.\n" ++ (show f'))
    putStrLn ""
    setSGR [Reset]
