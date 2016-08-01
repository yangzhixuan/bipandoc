module HTMLQuickCheck where

import Parser.Markdown
import Parser.HTMLParser
import BX.HTMLBX
import BX.MarkdownBX
import Abstract

import Data.Char (isSpace, ord)
import Data.Maybe
import Control.Monad
import Test.QuickCheck.Gen
import Test.QuickCheck
import Text.Show.Pretty


import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Debug.Trace

testArgs :: Args
testArgs = stdArgs {maxSuccess=600}


cstPrintParse :: IO ()
cstPrintParse = quickCheckWith testArgs prop_CSTPrintParse

html_md_html_round :: IO ()
html_md_html_round = quickCheckWith testArgs prop_HTML_MD_HTML_round

html_md_html_round_allow_fail :: IO ()
html_md_html_round_allow_fail = quickCheckWith testArgs prop_HTML_MD_HTML_round_allow_fail

prop_HTML_MD_HTML_round :: HTMLDoc -> Bool
prop_HTML_MD_HTML_round htmlCST =
  let htmlTXT = prtDocument htmlCST
      ast     = maybe (error "error in (get htmlBX)") id (get htmlBX htmlCST)
      mdCST   = maybe (error "error in (put markdownBX)") id (put markdownBX emptyMD ast)
      mdTXT   = printMarkdown mdCST
      mdCST'  = parseMarkdown mdTXT
      ast'    = maybe (error "error in (get markdownBX)") id (get markdownBX mdCST')
      htmlCST'= maybe (error "error in (put htmlBX)") id (put htmlBX htmlCST ast')
      htmlTXT'= prtDocument htmlCST'
  in  htmlTXT == htmlTXT'
  where types = htmlCST :: HTMLDoc
        emptyMD = MarkdownDoc []

prop_HTML_MD_HTML_round_allow_fail :: HTMLDoc -> Bool
prop_HTML_MD_HTML_round_allow_fail htmlCST =
    fromMaybe (trace "\na bigul fail\n" True) $ 
      do let htmlTXT = prtDocument htmlCST
         ast <- get htmlBX htmlCST
         mdCST <- put markdownBX emptyMD ast
         let mdTXT = printMarkdown mdCST
         let mdCST' = parseMarkdown mdTXT
         ast' <- get markdownBX mdCST'
         htmlCST' <- put htmlBX htmlCST ast'
         let htmlTXT' = prtDocument htmlCST'
         return $ htmlTXT' == htmlTXT
         where emptyMD = MarkdownDoc []
       

test2 :: HTMLDoc -> IO ()
test2 htmlCST = do
  let ast     = maybe (error "error in (get htmlBX)") id (get htmlBX htmlCST)
  putStrLn "original AST\n"
  putPretty ast
  writeFile "test-original-ast.txt" (ppShow ast)
  let mdCST   = maybe (error "error in (put markdownBX)") id (put markdownBX emptyMD ast)
  putStrLn "\nmarkdown CST:\n"
  putPretty mdCST

  let mdTXT   = printMarkdown mdCST
  putStrLn "\nmarkdown text:\n"
  putStrLn mdTXT
  writeFile "test-markdown.md" mdTXT

  let mdCST'  = parseMarkdown mdTXT
--  putStrLn "\nmarkdown CST (new):\n"
--  putPretty mdCST'

  let ast'    = maybe (error "error in (get markdownBX)") id (get markdownBX mdCST')
  putStrLn "\nAST (new):\n"
  putPretty ast'
  writeFile "test-new-ast.txt" (ppShow ast')
  putStr "\nView eq to original view? :"
  print (ast' == ast)
  putStrLn ""
  where
        emptyMD = MarkdownDoc []




prop_CSTPrintParse :: HTMLDoc -> Bool
prop_CSTPrintParse htmlDoc = parseHTML (prtDocument htmlDoc) == htmlDoc
  where types = htmlDoc :: HTMLDoc


genHTML :: Gen HTML
genHTML = sized genHTML'

genHTML' :: Int -> Gen HTML
genHTML' n = do
  tags <- listOf1 (resize 8 $ genBlockTag' (n `div` 2))
  tags' <- noConsecutiveLists (n `div` 2) tags
  return $ GTree (CTag Block (Right "html") [] NormalClose)
           [GTree (CTagText OtherText (TR "\n")) []
           ,GTree (CTag Block (Right "head") [] NormalClose) [GTree (CTagText OtherText (TR "\n")) []]
           ,GTree (CTagText OtherText (TR "\n  ")) []
           ,GTree (CTag Block (Right "body") [] NormalClose) tags']

genBlockTag' :: Int -> Gen (GTree CTag)
genBlockTag' 0 = return $ GTree (CTagText OtherText (TR "\n")) []
genBlockTag' n | n > 0 =
  frequency [(1, genComments)
            -- ,(1, return $ GTree (CTagText OtherText (Right "\n")) [])
            --,(1, resize 8 $ genDIV' (n `div` 2) )
            ,(1, genBLOCKCODE)
            ,(1, resize 8 $ genP' (n `div` 2) )
            ,(1, resize 8 $ genUL' (n `div` 2) )
            ,(1, resize 8 $ genOL' (n `div` 2) )
            ,(1, resize 8 $ genHEAD' (n `div` 2) )]

genComments :: Gen (GTree CTag)
genComments = genSafeString >>= \s -> return $ GTree (CTagComment s) []

genDIV' :: Int -> Gen (GTree CTag)
genDIV' n = do {tags <- listOf1 (genBlockTag' (n `div` 2) ); return $ GTree (CTag Block (Left CDiv) [] NormalClose) tags}

genBLOCKCODE :: Gen (GTree CTag)
genBLOCKCODE =
  genSafeString >>= \s -> return $
      GTree (CTag Block (Left CPre) [] NormalClose)
            [GTree (CTag Block (Left CCode) [] NormalClose)
                   [GTree (CTagCode (s ++ "\n")) []]]

genINLINECODE :: Gen (GTree CTag)
genINLINECODE = genSafeString >>= (\s -> return $ GTree (CTag Block (Left CCode) [] NormalClose) [GTree (CTagCode (s  ++ "\n")) []])

genUL' :: Int -> Gen (GTree CTag)
genUL' n = do {lis <- resize 8 $ listOf1 (genLI' (n `div` 2) ); return $ GTree (CTag Block (Left CUnorderedList) [] NormalClose) lis}

genOL' :: Int -> Gen (GTree CTag)
genOL' n = do {lis <- resize 8 $ listOf1 (genLI' (n `div` 2) ); return $ GTree (CTag Block (Left COrderedList) [] NormalClose) lis}

noConsecutiveLists :: Int -> [GTree CTag] -> Gen [GTree CTag]
noConsecutiveLists n [] = return []
noConsecutiveLists n [x] = return $ [x]
noConsecutiveLists n (l1@(GTree (CTag _ (Left COrderedList) _ _) _) : l2@(GTree (CTag _ (Left COrderedList) _ _) _) : res) = 
    do p <- genP' (n `div` 2)
       l' <- noConsecutiveLists (n `div` 2) (l2 : res)
       return $ l1 : p : l'
noConsecutiveLists n (l1@(GTree (CTag _ (Left CUnorderedList) _ _) _) : l2@(GTree (CTag _ (Left CUnorderedList) _ _) _) : res) = 
    do p <- genP' (n `div` 2)
       l' <- noConsecutiveLists (n `div` 2) (l2 : res)
       return $ l1 : p : l'
noConsecutiveLists n (l1 : res) = 
    do l' <- noConsecutiveLists (n `div` 2) res
       return $ l1:l'

genLI' :: Int -> Gen (GTree CTag)
genLI' n = do
  attrs <- listOf genAttributeWithSpace
  blocks <- resize 4 $ listOf (genBlockTag' (n `div` 2) )
  blocks' <- noConsecutiveLists (n `div` 2) blocks
  return $ GTree (CTag Block (Left CListItem) (concat attrs) NormalClose) blocks'

genP' :: Int -> Gen (GTree CTag)
genP' n = do
  attrs <- listOf genAttributeWithSpace
  inlines <- resize 4 $ listOf1 (genInline' (n `div` 2) )
  return $ GTree (CTag Block (Left CPara) (concat attrs) NormalClose) ( GTree (CTagText InlineText (TR "nonempty")) [] : inlines)

genHEAD' :: Int -> Gen (GTree CTag)
genHEAD' n = do
  attrs <- resize 4 $ listOf genAttributeWithSpace
  inlines <- listOf1 (genInline' (n `div` 2) )
  level <- choose (1,6)
  return $ GTree (CTag Block (Left (CHead level)) (concat attrs) NormalClose) inlines

genAttributeWithSpace :: Gen [Either Spaces Attribute]
genAttributeWithSpace = do
  spaces <- genSpaces
  attr <- otherAttr
  return [Left spaces, Right attr]
  where otherAttr :: Gen Attribute
        otherAttr = do
          name <- genLowerString
          val <- stringInQuote
          return $ Attribute name " = " val

genInline' :: Int -> Gen (GTree CTag)
genInline' 0 = return $ GTree (CTagText InlineText (TR "this is not a space? ")) []
genInline' n | n > 0 =
  frequency [(6, genInlineText)
--            ,(1, genComments)
            ,(1, genSTRONG' (n `div` 4))
            --,(1, genEMPH' (n `div` 4))
            ,(1, genINLINECODE)
            ,(1, genIMG)
            ,(1, genLINK' (n `div` 4))]


genInlineText :: Gen (GTree CTag)
genInlineText = do {txt <- strWithoutSpace; return $ GTree (CTagText InlineText (TR txt)) []}

genSTRONG' :: Int -> Gen (GTree CTag)
genSTRONG' n = do {inlines <- resize 8 $ listOf1 (genInline' (n `div` 4)); attrs <- listOf genAttributeWithSpace; return $ GTree (CTag Inline (Left CStrong) (concat attrs) NormalClose) inlines}

genEMPH' :: Int -> Gen (GTree CTag)
genEMPH' n = do {inlines <- resize 8 $ listOf1 (genInline' (n `div` 4)); attrs <- listOf genAttributeWithSpace; return $ GTree (CTag Inline (Left CEmph) (concat attrs) NormalClose) inlines}

genIMG :: Gen (GTree CTag)
genIMG = do
  srcVal <- genSafeString
  altVal <- genSafeString
  return $ GTree (CTag Inline (Left CImg) [Left " ", Right (Attribute "src" "=" (addQuotes "" srcVal)), Left "  ", Right (Attribute "alt" "=" (addQuotes "" altVal)) ] NoClose) []

genLINK' :: Int -> Gen (GTree CTag)
genLINK' n = do
  inlineTxt <- resize 5 $ listOf1 (genInline' (n `div` 4))
  hrefVal <- genSafeString
  return $ GTree (CTag Inline (Left CLink) [Left " ", Right (Attribute "href" "=" (addQuotes "" hrefVal))] NormalClose) inlineTxt

-- hard break. <br>
genBR :: Gen (GTree CTag)
genBR = return $ GTree (CTag Inline (Left CBr) [] NoClose) []

-- soft break. \n
genSOFTBREAK :: Gen (GTree CTag)
genSOFTBREAK = return $ GTree (CTagText InlineText (TM "\n")) []

-- a sequence of spaces of length 1 - 10
genSpaceInlineText :: Gen (GTree CTag)
genSpaceInlineText = do {spaces <- genSpaces; return $ GTree (CTagText InlineText (TM spaces)) []}



genSpaces :: Gen String
genSpaces = elements $ zipWith (\s n -> concat $ replicate n s) (repeat " ") [1..10]

stringInQuote :: Gen String
stringInQuote = genSafeString >>= \s -> return $ "\"" ++ s ++ "\""

strWithoutSpace :: Gen String
strWithoutSpace = suchThat genSafeString (\s -> not (any isSpace s) && not (null s))


genCharNum :: Gen Char
genCharNum = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

genSafeString :: Gen String
genSafeString = listOf1 genCharNum

genLowerChar :: Gen Char
genLowerChar = elements $ ['a'..'z']

genLowerString :: Gen String
genLowerString = listOf1 genLowerChar

instance Arbitrary HTMLDoc where
  arbitrary = do
    html <- genHTML
    return . parseHTML . prtDocument $ HTMLDoc " " "<!DOCTYPE HTML>" "\n" html "\n"


