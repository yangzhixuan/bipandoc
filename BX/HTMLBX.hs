{-# Language TemplateHaskell #-}

module BX.HTMLBX where

import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Interpreter

import Abstract
import BX.BXHelpers

import Parser.HTMLParser
import Text.Megaparsec
import qualified System.IO.Strict as IOS



htmlBX :: BiGUL Document AbsDocument
htmlBX = $(update [p| Document _ _ _ html _ |] [p| html |]
                  [d| html = blockListHTML |])

blockListHTML :: BiGUL HTML AbsDocument
blockListHTML =
  Case  [ $(normalSV [p| GTree (CTag Block (Right "html") _ NormalClose) _ |] [p| AbsDocument _ |]
                     [p| GTree (CTag Block (Right "html") _ NormalClose) _ |] ) $
            $(update [p| GTree _ subs  |] [p| AbsDocument subs |] [d| subs = refineBX1 |])
        ]

refineBX1 :: BiGUL [GTree CTag] [AbsBlock]
refineBX1 =
  Case  [ $(normalSV [p| GTree (CTag Block (Right "head") _ NormalClose) _   : _ |] [p| _ |]
                     [p| GTree (CTag Block (Right "head") _ NormalClose) _   : _ |]) $
             $(update [p| _:next |] [p| next |] [d| next = refineBX1 |])
        , $(normalSV [p| GTree (CTagText OtherText _) []  : _ |] [p| _ |]
                     [p| GTree (CTagText OtherText _) []  : _ |]) $
             $(update [p| _:next |] [p| next |] [d| next = refineBX1 |])
        , $(normalSV [p| GTree (CTag Block (Right "body") _ NormalClose) _   : _ |] [p| _ |]
                     [p| GTree (CTag Block (Right "body") _ NormalClose) _   : _ |]) $
            $(rearrV [| \v -> (v,[]) |]) $
               $(update [p| (GTree (CTag Block (Right "body") _ NormalClose) subs) : rem |] [p| (subs,rem) |]
                        [d| subs = (filterLens isSupportedNode) `Compose` blockListBX; rem = refineBX1 |])
        , $(normalSV [p| [] |] [p| [] |] [p| [] |]) $(update [p| [] |] [p| [] |] [d|  |])
        , $(adaptiveSV [p| _:_ |] [p| [] |]) (\_ _ -> [])
        ]


---- delete comment tag, script tag, and someother unsupported tags
---- filterLens is not enough. we need something like "fold filterLens"
--  where createBlock = const $ (GTree (CTag Block (Left CDiv) [] "div") [GTree (CTag Block (Left CPara) [] NormalClose) [] ])
--        -- this function should be refined later

-- test this idea: skip dividing blocks such as "div", "span" ...
blockListBX :: BiGUL [GTree CTag] [AbsBlock]
blockListBX =
  Case  [ $(normalSV [p| [] |] [p| [] |] [p| [] |] ) ==> $(update [p| [] |] [p| [] |] [d|  |])

        -- take the elements in "div" "span", etc. out
        , $(normalSV [p| GTree (CTag Block (Left CDiv) _ NormalClose) (_:_) : _ |] [p| _:_ |]
                     [p| GTree (CTag Block (Left CDiv) _ NormalClose) (_:_) : _ |] ) $
              $(rearrS [| \( (GTree (CTag Block (Left CDiv) attrs NormalClose) (c:contents)) : blocks) ->
                           (c, (GTree (CTag Block (Left CDiv) attrs NormalClose) contents) : blocks) |]) $
                $(update [p| (c, cbs) |] [p| c:cbs |]
                         -- problematic. c may be a comment node which should be filtered out.
                         [d| c = blockBX; cbs = blockListBX |])

        -- drop the empty "div", etc.
        , $(normalSV [p| (GTree (CTag Block (Left CDiv) _ NormalClose) []) : _ |] [p| _:_ |]
                     [p| (GTree (CTag Block (Left CDiv) _ NormalClose) []) : _ |] ) $
              $(rearrS [| \( (GTree (CTag Block (Left CDiv) attrs NormalClose) []) : blocks) -> blocks  |]) $
                $(update [p| cbs |] [p| cbs |] [d| cbs = blockListBX |])

        -- span not handled yet

        -- skip the (CTagText OtherText _) node
        , $(normalSV [p| GTree (CTagText OtherText _) [] : _ |] [p| _ |] [p| _:_ |] ) $
            $(update [p| _:cbs |] [p| cbs |] [d| cbs = blockListBX |])

        -- skip the (CTagScript _) node
        , $(normalSV [p| GTree (CTagScript _) [] : _ |] [p| _ |] [p| _:_ |] ) $
            $(update [p| _:cbs |] [p| cbs |] [d| cbs = blockListBX |])

        -- skip the (CTagComment _) node
        , $(normalSV [p| GTree (CTagComment _) [] : _ |] [p| _ |] [p| _:_ |] ) $
            $(update [p| _:cbs |] [p| cbs |] [d| cbs = blockListBX |])

        -- normal inductive cases
        , $(normalSV [p| _:_ |] [p| _:_ |] [p| _:_ |] ) $
            $(update [p| c:cbs |] [p| c:cbs |] [d| c = blockBX; cbs = blockListBX |])

          -- delete extra source elements
        , $(adaptiveSV [p| _:_ |] [p| [] |] ) (\_ _ -> [])

          -- add more source elements
        , $(adaptiveSV [p| [] |] [p| _:_ |] ) (\_ v -> replicate (length v) (GTree CDefaultTag []))
        ]




-- we follow the idea similar to lensMap: the strategy is matching by position.
-- we could in the future use align to calculate similarity between source block and view block.
-- Then we may need to move adaptation statements to the end of the group.
blockBX :: BiGUL (GTree CTag) AbsBlock
blockBX =
  Case [ -- Case: AbsPara
         $(normalSV [p| GTree (CTag Block (Left CPara) _ NormalClose) _ |] [p| AbsPara _ |] [p| GTree (CTag Block (Left CPara) _ NormalClose) _ |])
         ==> $(update [p| GTree (CTag Block (Left CPara) _ NormalClose) xs |] [p| AbsPara xs |] [d| xs = mapLens inlineBX createInline |])

       , $(adaptiveSV [p| _ |] [p| AbsPara _ |])
         ==> \_ _ -> GTree (CTag Block (Left CPara) [] NormalClose) []

         -- Case: Heading
       , $(normalSV [p| GTree (CTag Block (Left (CHead _ )) _ NormalClose) _ |] [p| AbsHeading _ _ |] [p| GTree (CTag Block (Left (CHead _ )) _ NormalClose) _ |])
         ==> $(update [p| GTree (CTag Block (Left (CHead level )) _ NormalClose) contents |] [p| AbsHeading level contents |]
                      [d| level = Replace; contents = mapLens inlineBX createInline |])

       , $(adaptiveSV [p| _ |] [p| AbsHeading _ _ |])
         ==> \_ _ -> GTree (CTag Block (Left (CHead 1)) [] NormalClose) []

         -- Case: AbsUnorderedList
       , $(normalSV [p| GTree (CTag Block (Left CUnorderedList) _ NormalClose) _ |] [p| AbsUnorderedList _ |] [p| GTree (CTag Block (Left CUnorderedList) _ NormalClose) _ |])
         ==> $(update [p| GTree (CTag Block (Left CUnorderedList) _ NormalClose) items |] [p| AbsUnorderedList items |]
                      [d| items = filterLens isSupportedNode `Compose` mapLens unorderedListItemBX createListItem |])

       , $(adaptiveSV [p| _ |] [p| AbsUnorderedList _ |])
         ==> \_ _ -> GTree (CTag Block (Left CUnorderedList) [] NormalClose) []

         -- Case: AbsOrderedList
       , $(normalSV [p| GTree (CTag Block (Left COrderedList) _ NormalClose) _ |] [p| AbsOrderedList _ |] [p| GTree (CTag Block (Left COrderedList) _ NormalClose) _ |])
         ==> $(update [p| GTree (CTag Block (Left COrderedList) _ NormalClose) items |] [p| AbsOrderedList items |]
                      [d| items = filterLens isSupportedNode `Compose` mapLens orderedListItemBX createListItem |])

       , $(adaptiveSV [p| _ |] [p| AbsOrderedList _ |])
         ==> \_ _ -> GTree (CTag Block (Left COrderedList) [] NormalClose) []

         -- Case: AbsBlockQuote
       , $(normalSV [p| GTree (CTag Block (Left CBlockQuote) _ NormalClose) _ |] [p| AbsBlockQuote _ |] [p| GTree (CTag Block (Left CBlockQuote) _ NormalClose) _ |])
         ==> $(update [p| GTree (CTag Block (Left CBlockQuote) _ NormalClose) blocks |] [p| AbsBlockQuote blocks |]
                      [d| blocks = filterLens isSupportedNode `Compose` blockListBX |])

       , $(adaptiveSV [p| _ |] [p| AbsBlockQuote _ |])
         ==> \s v -> GTree (CTag Block (Left CBlockQuote) [] NormalClose) []

         -- Case: AbsCode
       , $(normalSV [p| GTree (CTag Block (Left CCode) _ NormalClose) [ GTree (CTagText InlineText _) [] ] |]
                    [p| AbsCode _ |] [p| GTree (CTag Block (Left CCode) _ NormalClose) [ GTree (CTagText InlineText _) [] ] |])
         ==> $(update [p| GTree (CTag Block (Left CCode) _ NormalClose) [GTree (CTagText InlineText someCode) [] ] |] [p| AbsCode someCode |]
                      [d| someCode = Replace |])
       , $(adaptiveSV [p| _ |] [p| AbsCode _ |])
         ==> \_ _ -> GTree (CTag Block (Left CCode) [] NormalClose) [ GTree (CTagText InlineText "") [] ]

       ]
  where --setextLineBX = emb (\s -> if head s == '=' then 1 else 2)
                          --(\line level -> replicate (length line) (if level == 1 then '=' else '-'))
        createInline = const $ GTree (CTagText InlineText "hehe") []


--createListItem = undefined
createListItem :: AbsListItem -> GTree CTag
createListItem (AbsUnorderedListItem _) = GTree (CTag Block (Left CUnorderedList) [] NormalClose) []
createListItem (AbsOrderedListItem _)   = GTree (CTag Block (Left COrderedList)   [] NormalClose) []

-- other text not handled. so error occured.
unorderedListItemBX :: BiGUL (GTree CTag) AbsListItem
unorderedListItemBX = $(update [p| GTree (CTag Block (Left CListItem) _ _) blocks |] [p| AbsUnorderedListItem blocks |]
                               [d| blocks = (filterLens isSupportedNode) `Compose` blockListBX |])

--orderedListItemBX = undefined
orderedListItemBX :: BiGUL (GTree CTag) AbsListItem
orderedListItemBX = $(update [p| GTree (CTag Block (Left CListItem) _ _ ) blocks |] [p| AbsOrderedListItem blocks |]
                             [d| blocks = (filterLens isSupportedNode) `Compose` blockListBX |])


inlineBX :: BiGUL (GTree CTag) AbsInline
inlineBX =
  Case [ -- Case: AbsStr is a space
--         $(normal [| \(Spaces _) (AbsStr s) -> s == " " |] [| \(Spaces _) -> True |])
--           ==> Skip (const (AbsStr " "))

--         , $(adaptive [| \_ (AbsStr s) -> s == " " |])
--           ==> \s v -> (Spaces " ")

--           -- Case: AbsStr is a punctuation
--         , $(normal [| \(EscapedCharInline _) (AbsStr s) -> length s == 1 && head s `elem` punctuation |]
--                    [| \(EscapedCharInline _) -> True |])
--           ==> $(update [p| EscapedCharInline c |] [p| AbsStr (c : []) |]
--                        [d| c = Replace |])

--         , $(adaptive [| \_ (AbsStr s) -> length s == 1 && head s `elem` punctuation |])
--           ==> \s v -> (EscapedCharInline ' ')

         -- Case: AbsStr
         $(normalSV [p| GTree (CTagText InlineText _) [] |] [p| AbsStr _ |] [p| GTree (CTagText InlineText _) [] |])
         ==> $(update [p| GTree (CTagText InlineText str) [] |] [p| AbsStr str |] [d| str = Replace |])

       , $(adaptiveSV [p| _ |] [p| AbsStr _ |])
         ==> \_ (AbsStr str) -> GTree (CTagText InlineText str) []

         -- Case: Emph
       , $(normalSV [p| GTree (CTag Inline (Left CEmph) _ NormalClose) _ |] [p| AbsEmph _ |] [p| GTree (CTag Inline (Left CEmph) _ NormalClose) _ |])
         ==> $(update [p| GTree (CTag Inline (Left CEmph) _ NormalClose) subs |] [p| AbsEmph subs |]
                      [d| subs = filterLens isSupportedNode `Compose` mapLens inlineBX createInline |])

       , $(adaptiveSV [p| _ |] [p| AbsEmph _ |])
         ==> \_ _ -> GTree (CTag Inline (Left CEmph) [] NormalClose) []

--           -- Case: Strong
--         , $(normalSV [p| Strong _ |] [p| AbsStrong _ |] [p| Strong _ |])
--           ==> $(update [p| Strong x |] [p| AbsStrong x |]
--                        [d| x = mapLens inlineBX createInline |])

--         , $(adaptiveSV [p| _ |] [p| AbsStrong _ |])
--           ==> \_ v -> Strong []

--           -- Case: Softbreak
--         , $(normalSV [p| Softbreak _ |] [p| AbsSoftbreak |] [p| Softbreak _ |])
--           ==> Skip (const AbsSoftbreak)

--              -- NOTE: the correct indentation is infered later
--         , $(adaptiveSV [p| _ |] [p| AbsSoftbreak |])
--           ==> \s v -> Softbreak ""

--           -- Case: Hardbreak
--         , $(normalSV [p| Hardbreak _ _ |] [p| AbsHardbreak |] [p| Hardbreak _ _|])
--           ==> Skip (const AbsHardbreak)

--         , $(adaptiveSV [p| _ |] [p| AbsHardbreak |])
--           ==> \s v -> Hardbreak "  " ""

--           -- Case: Code
--         , $(normalSV [p| InlineCode _ _ |] [p| AbsInlineCode _ |] [p| InlineCode _ _ |])
--           ==> $(update [p| InlineCode _ c |] [p| AbsInlineCode c |]
--                        [d| c = Replace |])

--         , $(adaptiveSV [p| _ |] [p| AbsInlineCode _ |])
--           ==> \s v -> (InlineCode "`" "")

--           -- Case: AbsLink
--         , $(normalSV [p| Link _ _ |] [p| AbsLink _ _ |] [p| Link _ _ |])
--           ==> $(update [p| Link t dest |] [p| AbsLink t dest |]
--                        [d| t = mapLens inlineBX createInline; dest = Replace |])

--         , $(adaptiveSV [p| _ |] [p| AbsLink _ _ |])
--           ==> \s v -> (Link [] "")

--           -- Case: AbsImage
--         , $(normalSV [p| Image _ _ |] [p| AbsImage _ _ |] [p| Image _ _ |])
--           ==> $(update [p| Image t dest |] [p| AbsImage t dest |]
--                        [d| t = Replace; dest = Replace |])

--         , $(adaptiveSV [p| _ |] [p| AbsImage _ _ |])
--           ==> \s v -> (Image "" "")
       ]
  where createInline = const $ (GTree (CTagText InlineText "") [])

testFromHTMLG :: IO ()
testFromHTMLG = do
  i <- IOS.readFile "1.html"
  let o  = either (error. show) id (parse parseDoc "1.html" i)
      oo = refineDoc o
      ast = maybe (error "nothing") id (get htmlBX oo)
  putStrLn (show ast)


testFromHTMLG' :: IO ()
testFromHTMLG' = do
  i <- readFile "1.html"
  let o  = either (error. show) id (parse parseDoc "1.html" i)
      oo = refineDoc o
      ast = (getTrace htmlBX oo)
  putStrLn (show ast)


testFromHTMLP :: IO ()
testFromHTMLP = do
  i <- IOS.readFile "1.html"
  let o  = either (error. show) id (parse parseDoc "1.html" i)
      oo = refineDoc o
      ast = maybe (error "nothing") id (get htmlBX oo)
      s'  = maybe (error "nothing") id (put htmlBX oo ast)
  putStrLn (prtDocument s')

testFromHTMLP2 :: IO ()
testFromHTMLP2 = do
  i <- IOS.readFile "1.html"
  let o  = either (error. show) id (parse parseDoc "1.html" i)
      oo = refineDoc o
      ast = maybe (error "nothing") id (get htmlBX oo)
      newS = Document ""  doctype " " html "\n"
      s'  = maybe (error "nothing") id (put htmlBX newS ast)
  putStrLn (prtDocument s')
  where doctype = "<!DOCTYPE HTML>"
        html    = (GTree (CTag Block (Right "html") [] NormalClose) [GTree (CTagText OtherText "\n") [],GTree (CTag Block (Right "head") [] NormalClose) [GTree (CTagText OtherText "\n") []],GTree (CTagText OtherText "\n  ") [],GTree (CTag Block (Right "body") [] NormalClose) []])

