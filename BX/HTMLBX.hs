{-# Language TemplateHaskell #-}

module BX.HTMLBX where

import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Interpreter

import Abstract
import BX.BXHelpers

import Parser.HTMLParser
import Parser.HTMLParserDataType
import Text.Megaparsec hiding (State)
import qualified System.IO.Strict as IOS
import Control.Monad.State as State

import Data.Char (isSpace)

import Debug.Trace

htmlBX :: BiGUL HTMLDoc AbsDocument
htmlBX = $(update [p| HTMLDoc _ _ _ html _ |] [p| html |]
                  [d| html = filterGTree isSupportedNode `Compose` blockListHTML |])


blockListHTML :: BiGUL HTML AbsDocument
blockListHTML =
  Case  [ $(normalSV [p| GTree (CTag Block (Right "html") _ NormalClose) _ |] [p| AbsDocument _ |]
                     [p| GTree (CTag Block (Right "html") _ NormalClose) _ |] ) $
            $(update [p| GTree _ subs  |] [p| AbsDocument subs |] [d| subs = refineBX1 |])
        ]

refineBX1 :: BiGUL [GTree CTag] [AbsBlock]
refineBX1 =
  Case  [ $(normalSV [p| GTree (CTag Block (Right "body") _ NormalClose) _ : [] |] [p| _ |]
                     [p| GTree (CTag Block (Right "body") _ NormalClose) _ : [] |]) $
            $(update [p| [body] |] [p| body |] [d| body = lensFlattenDiv `Compose` blockListBX |])
        ]

-- kind of filter-GTree, but do not test the given node. The filter actually statrs from its children list.
-- filterGTree :: (Eq a, Show a) => (GTree a -> Bool) -> BiGUL (GTree a) (GTree a)
-- filterGTree p = $(update [p| GTree node subs |] [p| GTree node subs |]
--                          [d| node = Replace; subs = filterLens p `Compose` mapLens (filterGTree p) id |])

filterGTree p =
    Case [ $(normalSV [p| _ |] [p| (GTree (CTagText InlineText _) []) |]
                      [p| (GTree (CTagText InlineText _) []) |] )
           ==> Replace

         , $(normalSV [p| _ |] [p| (GTree (CTagCode _) []) |]
                      [p| (GTree (CTagCode _) []) |] )
           ==> Replace

         , $(normal [| \s v -> True |] [| const True |])
           ==> $(update [p| GTree node subs |] [p| GTree node subs |]
                         [d| node = Replace; subs = filterLens p `Compose` mapLens (filterGTree p) id |])
         ]


-- test this idea: skip dividing blocks such as "div", "span" ...
blockListBX :: BiGUL [GTree CTag] [AbsBlock]
blockListBX =
  Case  [ $(normalSV [p| [] |] [p| [] |] [p| [] |] ) ==> $(update [p| [] |] [p| [] |] [d|  |])

        -- span not handled yet
        -- normal inductive cases
        , $(normalSV [p| _:_ |] [p| _:_ |] [p| _:_ |] ) $
            $(update [p| c:cbs |] [p| c:cbs |] [d| c = blockBX; cbs = blockListBX |])

          -- delete extra source elements
        , $(adaptiveSV [p| _:_ |] [p| [] |] ) (\_ _ -> [])

          -- add more source elements
        , $(adaptiveSV [p| [] |] [p| _:_ |] ) (\_ v -> [createBlock (head v)])
        ]


-- we follow the idea similar to lensMap: the strategy is matching by position.
-- we could in the future use align to calculate similarity between source block and view block.
-- Then we may need to move adaptation statements to the end of the group.
blockBX :: BiGUL (GTree CTag) AbsBlock
blockBX =
  Case [ -- Case: AbsPara
         $(normalSV [p| GTree (CTag Block (Left CPara) _ NormalClose) _ |] [p| AbsPara _ |]
                    [p| GTree (CTag Block (Left CPara) _ NormalClose) _ |])
         ==> $(update [p| GTree (CTag Block (Left CPara) _ NormalClose) xs |] [p| AbsPara xs |]
                      [d| xs = mapLens inlineBX createInline `Compose` lensConcatEntityStr |])

         -- Case: Heading
       , $(normalSV [p| GTree (CTag Block (Left (CHead _ )) _ NormalClose) _ |] [p| AbsHeading _ _ |]
                    [p| GTree (CTag Block (Left (CHead _ )) _ NormalClose) _ |])
         ==> $(update [p| GTree (CTag Block (Left (CHead level )) _ NormalClose) contents |] [p| AbsHeading level contents |]
                      [d| level = Replace; contents = mapLens inlineBX createInline `Compose` lensConcatEntityStr |])

         -- Case: AbsUnorderedList
       , $(normalSV [p| GTree (CTag Block (Left CUnorderedList) _ NormalClose) _ |] [p| AbsUnorderedList _ |]
                    [p| GTree (CTag Block (Left CUnorderedList) _ NormalClose) _ |])
         ==> $(update [p| GTree (CTag Block (Left CUnorderedList) _ NormalClose) items |] [p| AbsUnorderedList items |]
                      [d| items = mapLens unorderedListItemBX createListItem |])

         -- Case: AbsOrderedList
       , $(normalSV [p| GTree (CTag Block (Left COrderedList) _ NormalClose) _ |] [p| AbsOrderedList _ |]
                    [p| GTree (CTag Block (Left COrderedList) _ NormalClose) _ |])
         ==> $(update [p| GTree (CTag Block (Left COrderedList) _ NormalClose) items |] [p| AbsOrderedList items |]
                      [d| items = mapLens orderedListItemBX createListItem |])

         -- Case: AbsBlockQuote
       , $(normalSV [p| GTree (CTag Block (Left CBlockQuote) _ NormalClose) _ |] [p| AbsBlockQuote _ |]
                    [p| GTree (CTag Block (Left CBlockQuote) _ NormalClose) _ |])
         ==> $(update [p| GTree (CTag Block (Left CBlockQuote) _ NormalClose) blocks |] [p| AbsBlockQuote blocks |]
                      [d| blocks = filterLens isSupportedNode `Compose` blockListBX |])

         -- Case: AbsCode. <pre><code> ... </code> </pre>
       , $(normalSV [p| GTree (CTag Block (Left CPre) _ NormalClose) [GTree (CTag Inline (Left CCode) _ NormalClose) [GTree (CTagCode _) [] ]] |] [p| AbsCode _ |]
                    [p| GTree (CTag Block (Left CPre) _ NormalClose) [GTree (CTag Inline (Left CCode) _ NormalClose) [GTree (CTagCode _) [] ]] |])
         ==> $(update [p| GTree (CTag Block (Left CPre) _ NormalClose) [GTree (CTag Inline (Left CCode) _ NormalClose) [GTree (CTagCode someCode) [] ]] |]
                      [p| AbsCode someCode |]
                      [d| someCode = Replace |])

       , $(adaptiveSV [p| _ |] [p| AbsPara _ |])
         ==> \_ _ -> GTree (CTag Block (Left CPara) [] NormalClose) []

       , $(adaptiveSV [p| _ |] [p| AbsHeading _ _ |])
         ==> \_ _ -> GTree (CTag Block (Left (CHead 1)) [] NormalClose) []

       , $(adaptiveSV [p| _ |] [p| AbsUnorderedList _ |])
         ==> \_ _ -> GTree (CTag Block (Left CUnorderedList) [] NormalClose) []

       , $(adaptiveSV [p| _ |] [p| AbsOrderedList _ |])
         ==> \_ _ -> GTree (CTag Block (Left COrderedList) [] NormalClose) []

       , $(adaptiveSV [p| _ |] [p| AbsBlockQuote _ |])
         ==> \s v -> GTree (CTag Block (Left CBlockQuote) [] NormalClose) []

       , $(adaptiveSV [p| _ |] [p| AbsCode _ |])
         ==> \_ _ -> GTree (CTag Block (Left CPre) [] NormalClose)
                           [GTree (CTag Inline (Left CCode) [] NormalClose) [ GTree (CTagCode "") [] ]]
       ]

createBlock :: AbsBlock -> GTree CTag
createBlock v = case v of
  AbsPara _ -> GTree (CTag Block (Left CPara) [] NormalClose) []
  AbsHeading _ _ -> GTree (CTag Block (Left (CHead 1)) [] NormalClose) []
  AbsUnorderedList _ -> GTree (CTag Block (Left CUnorderedList) [] NormalClose) []
  AbsOrderedList _ -> GTree (CTag Block (Left COrderedList) [] NormalClose) []
  AbsBlockQuote _ -> GTree (CTag Block (Left CBlockQuote) [] NormalClose) []
  AbsCode _ -> GTree (CTag Block (Left CPre) [] NormalClose)
                     [GTree (CTag Inline (Left CCode) [] NormalClose) [ GTree (CTagCode "") [] ]]


--createListItem = undefined
createListItem :: AbsListItem -> GTree CTag
createListItem (AbsUnorderedListItem _) = GTree (CTag Block (Left CListItem) [] NormalClose) []
createListItem (AbsOrderedListItem _)   = GTree (CTag Block (Left CListItem) [] NormalClose) []

-- other text not handled. so error occured.
unorderedListItemBX :: BiGUL (GTree CTag) AbsListItem
unorderedListItemBX = $(update [p| GTree (CTag Block (Left CListItem) _ _) blocks |] [p| AbsUnorderedListItem blocks |]
                               [d| blocks = blockListBX |])

--orderedListItemBX = undefined
orderedListItemBX :: BiGUL (GTree CTag) AbsListItem
orderedListItemBX = $(update [p| GTree (CTag Block (Left CListItem) _ _ ) blocks |] [p| AbsOrderedListItem blocks |]
                             [d| blocks = blockListBX |])


inlineBX :: BiGUL (GTree CTag) AbsInline
inlineBX =
  Case [ -- Case: Emph
         $(normalSV [p| GTree (CTag Inline (Left CEmph) _ NormalClose) _ |] [p| AbsEmph _ |]
                    [p| GTree (CTag Inline (Left CEmph) _ NormalClose) _ |])
         ==> $(update [p| GTree (CTag Inline (Left CEmph) _ NormalClose) subs |] [p| AbsEmph subs |]
                      [d| subs = mapLens inlineBX createInline `Compose` lensConcatEntityStr |])

         -- Case: Strong
       , $(normalSV [p| GTree (CTag Inline (Left CStrong) _ NormalClose) _ |] [p| AbsStrong _ |]
                    [p| GTree (CTag Inline (Left CStrong) _ NormalClose) _ |])
         ==> $(update [p| GTree (CTag Inline (Left CStrong) _ NormalClose) subs |] [p| AbsStrong subs |]
                      [d| subs = mapLens inlineBX createInline `Compose` lensConcatEntityStr |])

         -- Case: Softbreak in markdown.
       , $(normalSV [p| GTree (CTagText InlineText (TM "\n")) [] |] [p| AbsSoftbreak |]
                    [p| GTree (CTagText InlineText (TM "\n")) [] |])
         ==> Skip (const AbsSoftbreak)

         -- Case: Hardbreak in markdown. Maybe it is <br> in HTML
       , $(normalSV [p| GTree (CTag Inline (Left CBr) _ _) [] |] [p| AbsHardbreak |]
                    [p| GTree (CTag Inline (Left CBr) _ _) [] |])
         ==> Skip (const AbsHardbreak)

       -- Case: non-breaking space in html . &nbsp;
       , $(normalSV [p| GTree (CTagText InlineText (TL EntitySpace1)) [] |] [p| AbsStr " " |]
                    [p| GTree (CTagText InlineText (TL EntitySpace1)) [] |])
         ==> Skip (const (AbsStr " "))
      -- &#160;
       , $(normalSV [p| GTree (CTagText InlineText (TL EntitySpace2)) [] |] [p| AbsStr " " |]
                    [p| GTree (CTagText InlineText (TL EntitySpace2)) [] |])
         ==> Skip (const (AbsStr " "))

         -- Case: not soft break, not hard break. other spaces.
       , $(normalSV [| \(GTree (CTagText InlineText (TM str)) []) -> str /= "\n" |] [p| AbsStr " " |]
                    [| \(GTree (CTagText InlineText (TM str)) []) -> str /= "\n" |])
         ==> Skip (const (AbsStr " "))

          -- Case:  &lt;
       , $(normalSV [p| GTree (CTagText InlineText (TL EntityLT1)) [] |] [p| AbsStr "<" |]
                    [p| GTree (CTagText InlineText (TL EntityLT1)) [] |])
         ==> Skip (const (AbsStr "<"))
          -- "&#60;"
       , $(normalSV [p| GTree (CTagText InlineText (TL EntityLT2)) [] |] [p| AbsStr "<" |]
                    [p| GTree (CTagText InlineText (TL EntityLT2)) [] |])
         ==> Skip (const (AbsStr "<"))

          -- Case:  &gt;
       , $(normalSV [p| GTree (CTagText InlineText (TL EntityGT1)) [] |] [p| AbsStr ">" |]
                    [p| GTree (CTagText InlineText (TL EntityGT1)) [] |])
         ==> Skip (const (AbsStr ">"))
          -- "&#62;"
       , $(normalSV [p| GTree (CTagText InlineText (TL EntityGT2)) [] |] [p| AbsStr ">" |]
                    [p| GTree (CTagText InlineText (TL EntityGT2)) [] |])
         ==> Skip (const (AbsStr ">"))

          -- Case:  "&amp;"
       , $(normalSV [p| GTree (CTagText InlineText (TL EntityAmp1)) [] |] [p| AbsStr "&" |]
                    [p| GTree (CTagText InlineText (TL EntityAmp1)) [] |])
         ==> Skip (const (AbsStr "&"))
          -- "&#38;"
       , $(normalSV [p| GTree (CTagText InlineText (TL EntityAmp2)) [] |] [p| AbsStr "&" |]
                    [p| GTree (CTagText InlineText (TL EntityAmp2)) [] |])
         ==> Skip (const (AbsStr "&"))

          -- Case: AbsStr. not a whitespace string
       , $(normalSV [p| GTree (CTagText InlineText (TR _)) [] |] [p| AbsStr _ |]
                    [p| GTree (CTagText InlineText (TR _)) [] |])
         ==> $(update [p| GTree (CTagText InlineText (TR str)) [] |] [p| AbsStr str |]
                      [d| str = Replace |])

         -- Case: Code <code> ... </code>
       , $(normalSV [p| GTree (CTag Inline (Left CCode) _ NormalClose) _ |] [p| AbsInlineCode _ |]
                    [p| GTree (CTag Inline (Left CCode) _ NormalClose) _ |])
         ==> $(update [p| GTree (CTag Inline (Left CCode) _ NormalClose) [GTree (CTagCode c) []] |] [p| AbsInlineCode c |]
                      [d| c = Replace |])

         -- Case: AbsLink    <a href = "...">some text</a> . NOT the <link ...> tag!
       , $(normalSV [p| GTree (CTag Inline (Left CLink) _ NormalClose) _ |] [p| AbsLink _ _ |]
                    [p| GTree (CTag Inline (Left CLink) _ NormalClose) _ |])
         ==> $(update [p| GTree (CTag Inline (Left CLink) dest NormalClose) text |] [p| AbsLink text dest |]
                      [d| text = mapLens inlineBX createInline `Compose` lensConcatEntityStr ; dest = replaceHref |])

         -- Case: AbsImage
       , $(normalSV [p| GTree (CTag Inline (Left CImg) _ _) _ |] [p| AbsImage _ _ |]
                    [p| GTree (CTag Inline (Left CImg) _ _) _ |]) $
           $(rearrS [| \(GTree (CTag Inline (Left CImg) attrs _) []) -> attrs  |]) $
             $(rearrV [| \(AbsImage alt dest) -> (alt, dest)  |])
         ==> $(update [p| attrs2 |] [p| attrs2 |] [d| attrs2 = replaceAltAndDest |])


       , $(adaptiveSV [p| _ |] [p| AbsStr " " |])
         ==> \_ _ -> GTree (CTagText InlineText (TM " ")) []
       , $(adaptiveSV [p| _ |] [p| AbsStr "<" |])
         ==> \_ _ -> GTree (CTagText InlineText (TL EntityLT1)) []
       , $(adaptiveSV [p| _ |] [p| AbsStr ">" |])
         ==> \_ _ -> GTree (CTagText InlineText (TL EntityGT1)) []
       , $(adaptiveSV [p| _ |] [p| AbsStr "&" |])
         ==> \_ _ -> GTree (CTagText InlineText (TL EntityAmp1)) []
       , $(adaptiveSV [p| _ |] [p| AbsStr _ |])
         ==> \_ _ -> GTree (CTagText InlineText (TR "")) []

       , $(adaptiveSV [p| _ |] [p| AbsEmph _ |])
         ==> \_ _ -> GTree (CTag Inline (Left CEmph) [] NormalClose) []

       , $(adaptiveSV [p| _ |] [p| AbsStrong _ |])
         ==> \_ v -> GTree (CTag Inline (Left CStrong) [] NormalClose) []

            -- NOTE: the correct indentation is infered later
       , $(adaptiveSV [p| _ |] [p| AbsSoftbreak |])
         ==> \_ _ -> GTree (CTagText InlineText (TM "\n")) []

       , $(adaptiveSV [p| _ |] [p| AbsHardbreak |])
         ==> \s v -> GTree (CTag Inline (Left CBr) [] NoClose) []

       , $(adaptiveSV [p| _ |] [p| AbsInlineCode _ |])
         ==> \_ _ -> GTree (CTag Inline (Left CCode) [] NormalClose) [GTree (CTagCode "") []]

       , $(adaptiveSV [p| _ |] [p| AbsLink _ _ |])
         ==> \_ _ -> GTree (CTag Inline (Left CLink) [] NormalClose) []

       , $(adaptiveSV [p| _ |] [p| AbsImage _ _ |])
         ==> \_ _ -> GTree (CTag Inline (Left CImg) [] NoClose) []
       ]


createInline :: AbsInline -> GTree CTag
createInline v = case v of
  AbsStr " " -> GTree (CTagText InlineText (TM " ")) []
  AbsStr "<" -> GTree (CTagText InlineText (TL EntityLT1)) []
  AbsStr ">" -> GTree (CTagText InlineText (TL EntityGT1)) []
  AbsStr "&" -> GTree (CTagText InlineText (TL EntityAmp1)) []
  AbsStr _ -> GTree (CTagText InlineText (TR "newly created text to be replaced")) []
  AbsEmph _ -> GTree (CTag Inline (Left CEmph) [] NormalClose) []
  AbsStrong _ -> GTree (CTag Inline (Left CStrong) [] NormalClose) []
  AbsSoftbreak -> GTree (CTagText InlineText (TM "\n")) []
  AbsHardbreak -> GTree (CTag Inline (Left CBr) [] NoClose) []
  AbsInlineCode _ -> GTree (CTag Inline (Left CCode) [] NormalClose) [GTree (CTagCode "") []]
  AbsLink _ _  -> GTree (CTag Inline (Left CLink) [] NormalClose) []
  AbsImage _ _ -> GTree (CTag Inline (Left CImg) [] NoClose) []


replaceAltAndDest :: BiGUL [Either Spaces Attribute] (String, String)
replaceAltAndDest =
  emb (foldr foldrGetF ("",""))
      (\s (alt, src) -> foldr (foldrPutF alt src) [] (Left " ": Right (Attribute "src" "=" src) : Left " " : Right (Attribute "alt" "=" alt):s))
  where foldrGetF e (alt, src) = case e of
            Left _ -> (alt, src)
            Right (Attribute "src" _ src') -> (alt, dropQuotes src')
            Right (Attribute "alt" _ alt') -> (dropQuotes alt', src)
            Right _                     -> (alt, src)
        foldrPutF alt src x xs = case x of
            Left _ -> x:xs
            Right (Attribute "src" eq _) -> Right (Attribute "src" eq (addQuotes "" src)) : xs
            Right (Attribute "alt" eq _) -> Right (Attribute "alt" eq (addQuotes "" alt)) : xs
            Right _ -> x : xs

-- search for href attribute. and replace it with the view
replaceHref :: BiGUL [Either Spaces Attribute] String
replaceHref =
  Case  [ $(normalSV [p| Left _ : _ |] [p| _ |] [p| Left _ : _ |]) $
             $(update [p| _:xs |] [p| xs |] [d| xs = replaceHref |])
        , $(normalSV [p| Right (Attribute "href" _  _) : _ |] [p| _ |] [p| Right (Attribute "href" _  _) : _ |]) $
  --           $(rearrS [| \(Right (Attribute "href" eq  url) : rem)  -> Right ("href", (eq, url)) : rem  |]) $
               $(update [p| Right (Attribute _ _ url) : _ |] [p| url |] [d| url = replace_SourceInQuote|])
        , $(normalSV [p| Right _:_ |] [p| _ |] [p| Right _:_ |]) $
            $(update [p| _:xs |] [p| xs |] [d| xs = replaceHref |])

        , $(normalSV [p| [] |] [p| [] |] [p| [] |]) $
            $(update [p| [] |] [p| [] |] [d|  |])
        , $(adaptiveSV [p| [] |] [p| _:_ |]) (\_ v -> [Left " ", Right (Attribute "href" "=" (addQuotes "" v))] )
        ]


replace_SourceInQuote :: BiGUL String String
replace_SourceInQuote = emb dropQuotes addQuotes

-- the input string should be wrapped in quotes or doublequotes. drop the quotes
dropQuotes :: String -> String
dropQuotes ('\'':xs)  = take (length xs - 1) xs
dropQuotes ('"': xs)  = take (length xs - 1) xs
dropQuotes _        = error "the string does not start with quotes"


-- if the source string is in single-quotes, wrap the view string in single-quotes. The same for the source string in doublequotes.
-- otherwise add doublequotes as default case.
addQuotes :: String -> String -> String
addQuotes s v = case s of
  '\'':_  -> "'" ++ v ++ "'"
  '"':_   -> "\"" ++ v ++ "\""
  _       -> "\"" ++ v ++ "\""  -- default case.



lensFlattenDiv :: BiGUL (GTree CTag) [GTree CTag]
lensFlattenDiv = emb (flattenDivGet) (flattenDivPut)

flattenDivGet :: GTree CTag -> [GTree CTag]
flattenDivGet node = case node of
  GTree (CTag Block (Left CDiv) _ NormalClose) subtags -> concatMap flattenDivGet subtags
  b@(GTree (CTag Block (Left _) _ NormalClose) _) -> [b]
  GTree (CTag Block (Right "body") _ NormalClose) subtags -> concatMap flattenDivGet subtags
  _ -> error "error raised in flattenDivGet. The given source is not a block element."


flattenDivPut = \s v -> case State.evalState (flattenDivPut_ s) v of
  [tag] -> tag
  err   -> error "the result of flattenDivPut_ should be a singleton list"


-- concat [GTree CTag] will throw away the deleted elements (empty list)
flattenDivPut_ :: GTree CTag -> State [GTree CTag] [GTree CTag]
flattenDivPut_ node = case node of
  tag@(GTree (CTag Block (Left CDiv) attrs NormalClose) subtags) ->
    State.get >>= \v' -> case v' of
      (v:vs) -> do
        newsubtags <- mapM flattenDivPut_ subtags
        let remainV = drop (countBlockElement tag) v'
        State.put remainV
        return $ [GTree (CTag Block (Left CDiv) attrs NormalClose) (concat newsubtags)]
      []    -> return []
  tag@(GTree (CTag Block (Left _) _ NormalClose) _) ->
    State.get >>= \v' -> case v' of
      (v:vs) -> State.put vs >> return [v]
      []     -> return []
  tag@(GTree (CTag Block (Right "body") attrs NormalClose) subtags) ->
    State.get >>= \v' -> case v' of
      (v:vs) -> do
        newsubtags <- mapM flattenDivPut_ subtags
        let remainV = drop (countBlockElement tag) v'
        --traceM ("COUNTELEMENT:\n:" ++ show (countBlockElement tag))
        State.put remainV
        return [GTree (CTag Block (Right "body") attrs NormalClose) (concat newsubtags ++ remainV)]
      []    -> return $ [GTree (CTag Block (Right "body") attrs NormalClose) []]
  _ -> error "error raised in flattenDivPut_. The given source is not a block element."


countBlockElement :: GTree CTag -> Int
countBlockElement (GTree (CTag Block (Left CPara) _ _) _) = 1
countBlockElement (GTree (CTag Block (Left (CHead _)) _ _) _) = 1
countBlockElement (GTree (CTag Block (Left CBlockQuote) _ _) _) = 1
countBlockElement (GTree (CTag Block (Left CPre) _ _) _) = 1
countBlockElement (GTree (CTag Block (Left CUnorderedList) _ _) lis) = 1
countBlockElement (GTree (CTag Block (Left COrderedList) _ _) lis) = 1
countBlockElement (GTree (CTag Block (Left CDiv) _ _) subtags) = sum $ map countBlockElement subtags
countBlockElement (GTree (CTag Block (Right "body") attrs NormalClose) subtags) = sum $ map countBlockElement subtags
-- count <li> blocks are not counted



lensConcatEntityStr :: BiGUL [AbsInline] [AbsInline]
lensConcatEntityStr = emb concatEntityStr divideEntityStr

concatEntityStr :: [AbsInline] -> [AbsInline]
concatEntityStr (AbsStr x : AbsStr y : strs)
  | not (isSpace (head x)) && not (isSpace (head y)) = concatEntityStr (AbsStr (x ++ y) : strs)
  | otherwise = AbsStr x : concatEntityStr (AbsStr y : strs)
concatEntityStr (x : y : strs) = x : concatEntityStr (y:strs)
concatEntityStr strs = strs


divideEntityStr :: [AbsInline] -> [AbsInline] -> [AbsInline]
divideEntityStr _ viewStr = concatMap refine viewStr
  where refine :: AbsInline -> [AbsInline]
        refine (AbsStr str) =
          let (a,b) = break pred str
          in  if null b
                then if null a then [] else [AbsStr a]
                else if pred (head b)
                      then if null a then AbsStr [(head b)] : refine (AbsStr (tail b)) else AbsStr a : AbsStr [(head b)] : refine (AbsStr (tail b))
                      else error "should not reach here. divideEntityStr."
        refine a = [a]
        pred e = elem e ['<','>','&']
