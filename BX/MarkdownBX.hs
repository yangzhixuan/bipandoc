{-# Language TemplateHaskell, TypeFamilies #-}
module BX.MarkdownBX where

import Generics.BiGUL
import Data.List
import Data.Char
import qualified Data.Text as Text
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH

import BX.Library

import Abstract
import CST.Markdown

import Debug.Trace

markdownBX :: BiGUL MarkdownDoc AbsDocument
markdownBX = $(update [p| MarkdownDoc x |] [p| AbsDocument x |]
                      [d| x = blockListBX |])

-- A BX checking the validity of the AST. It can be composed with blockListBX
-- However it is not used in current program.
checkBX :: BiGUL [AbsBlock] [AbsBlock]
checkBX = 
    Case [
           $(normal [| \s v -> not (noConsecutiveLists v) |] [| const False |])
           ==> Fail (trace "fail consecutive lists" "consecutive lists are not allowed in markdown")

         , $(normalSV [p| _ |] [p| _ |] [p| _ |])
           ==> Replace
         ]
    where noConsecutiveLists [] = True
          noConsecutiveLists (x:[]) = True
          noConsecutiveLists ((AbsUnorderedList _) : (AbsUnorderedList _) : xs) = False
          noConsecutiveLists ((AbsOrderedList _) : (AbsOrderedList _) : xs) = False
          noConsecutiveLists (x:xs) = noConsecutiveLists xs



blockListBX :: BiGUL [Block] [AbsBlock]
blockListBX = (filterLens nonBlankLines) `Compose` (minEditDistLens blockBX createBlock) -- `Compose` checkBX
    where nonBlankLines (BlankLine _ _) = False
          nonBlankLines _ = True
          createBlock = const $ Para DefaultIndent []

blockBX :: BiGUL Block AbsBlock
blockBX =
    Case [ -- Case: AbsPara
           $(normalSV [p| Para _ _ |] [p| AbsPara _ |] [p| Para _ _ |])
           ==> $(update [p| Para _ x |] [p| AbsPara x |] [d| x = inlineListBX |])

         , $(adaptiveSV [p| _ |] [p| AbsPara _ |])
           ==> \_ v -> Para DefaultIndent []

           -- Case: AbsHeading
         , $(normalSV [p| ATXHeading{} |] [p| AbsHeading _ _ |] [p| ATXHeading{} |])
           ==> $(update [p| ATXHeading _ atx _ heading _ _ |] [p| AbsHeading atx heading |]
                        [d| atx = atxBX; heading = inlineListBX |])

         , $(normal [| \SetextHeading{} (AbsHeading level _) -> level == 1 || level == 2|] [| \SetextHeading{} -> True |])
           ==> $(update [p| SetextHeading _ heading _ _ setextLine _ |] [p| AbsHeading setextLine heading |]
                        [d| heading = inlineListBX; setextLine = setextLineBX |])

         , $(adaptiveSV [p| _ |] [p| AbsHeading _ _ |])
           ==> \_ v -> ATXHeading DefaultIndent "" " " [] "" "\n"

           -- Case: AbsUnorderedList
         , $(normalSV [p| UnorderedList _ |] [p| AbsUnorderedList _ |] [p| UnorderedList _ |])
           ==> $(update [p| UnorderedList items |] [p| AbsUnorderedList items |]
                        [d| items = minEditDistLens unorderedListItemBX createListItem |])

         , $(adaptiveSV [p| _ |] [p| AbsUnorderedList _ |])
           ==> \s v -> UnorderedList []

           -- Case: AbsOrderedList
         , $(normalSV [p| OrderedList _ |] [p| AbsOrderedList _ |] [p| OrderedList _ |])
           ==> $(update [p| OrderedList items |] [p| AbsOrderedList items |]
                        [d| items = minEditDistLens orderedListItemBX createListItem |])

         , $(adaptiveSV [p| _ |] [p| AbsOrderedList _ |])
           ==> \s v -> OrderedList []

           -- Case: AbsBlockQuote
         , $(normalSV [p| BlockQuote{} |] [p| AbsBlockQuote _ |] [p| BlockQuote{} |])
           ==> $(update [p| BlockQuote _ _ blocks |] [p| AbsBlockQuote blocks |]
                        [d| blocks = blockListBX |])

         , $(adaptiveSV [p| _ |] [p| AbsBlockQuote _ |])
           ==> \s v -> BlockQuote DefaultIndent ">" []

           -- Case: AbsCode
         , $(normalSV [p| IndentedCode _ |] [p| AbsCode _ |] [p| IndentedCode _ |])
           ==> $(update [p| IndentedCode codes |] [p| AbsCode codes |]
                        [d| codes = codeBX |])

         , $(normalSV [p| FencedCode{} |] [p| AbsCode _ |] [p| FencedCode{} |])
           ==> $(update [p| FencedCode _ _ _ codes _ _ _ |] [p| AbsCode codes |]
                        [d| codes = codeBX' |])

         , $(adaptiveSV [p| _ |] [p| AbsCode _ |])
           --- ==> \s v -> IndentedCode []
           ==> \s (AbsCode v) -> FencedCode DefaultIndent (generateFence v) "\n" [] DefaultIndent (generateFence v) "\n"
         ]
         where atxBX = emb length (\s v -> replicate v '#')
               setextLineBX = emb (\s -> if head s == '=' then 1 else 2)
                                  (\line level -> replicate (length line) (if level == 1 then '=' else '-'))

               -- FIXME Currently, we only allow code block terminating with a newline.
               -- This should be finished after migrating to super-view framework.
               codeBX = Case [ $(normal [| (\s v -> not (null v) && last v /= '\n') |] [| const False |] )
                                (Fail "markdown requires a newline as the last char in a code block"),

                               $(normal [| \_ _ -> True |] [| const True |])
                               ==> emb (concatMap (\(CodeLine ind code) -> code))
                                        (\s v -> map (\((CodeLine ind _), vc) -> CodeLine ind (vc ++ "\n"))
                                                     (zip (s ++ repeat (CodeLine DefaultIndent "\n")) (lines v)))
                             ]

               -- codeBX' always add a newline to code to support AbsCode terminating without a newline.
               codeBX' =  emb (removeNewline . concatMap (\(CodeLine ind code) -> code))
                              (\s v -> map (\((CodeLine ind _), vc) -> CodeLine ind (vc ++ "\n"))
                                           (zip (s ++ repeat (CodeLine DefaultIndent "\n")) (lines (v ++ "\n"))))
                   where removeNewline "" = ""
                         removeNewline s = init s

               generateFence str = tryFence "~~~"
                 where tryFence s = if Text.isInfixOf (Text.pack s) (Text.pack str)
                                       then tryFence ('~' : s)
                                       else s


createListItem :: AbsListItem -> ListItem
createListItem (AbsUnorderedListItem items) = UnorderedListItem DefaultIndent '*' " " (if null items then [BlankLine (Indent "") "\n"] else [])
createListItem (AbsOrderedListItem items) = OrderedListItem DefaultIndent DefaultItemNumber '.' " " (if null items then [BlankLine (Indent "") "\n"] else [])


unorderedListItemBX :: BiGUL ListItem AbsListItem
unorderedListItemBX = $(update [p| UnorderedListItem _ _ _ x |] [p| AbsUnorderedListItem x |]
                               [d| x = blockListBX |])

orderedListItemBX :: BiGUL ListItem AbsListItem
orderedListItemBX = $(update [p| OrderedListItem _ _ _ _ x |] [p| AbsOrderedListItem x |]
                               [d| x = blockListBX |])

inlineListBX :: BiGUL [Inline] [AbsInline]
inlineListBX = (minEditDistLens inlineBX createInline) `Compose` concatAbsStrLens

-- For AbsStr, we use EscapedCharInline as default, so that AbsStr '*' will be created as EscapedCharInline
createInline (AbsStr c) = if length c == 1 && head c `elem` "*-[]~`"
                             then EscapedCharInline '*'
                             else Str ""
createInline _ = Str ""

-- A convention in AST is that no consecutive non-space AbsStr should appear.
-- So we concatenate/split AbsStr in a lens.
concatAbsStrLens = emb concateAbsStr splitAbsStr

concateAbsStr :: [AbsInline] -> [AbsInline]
concateAbsStr (AbsStr x : AbsStr y : strs)
  | not (isSpace (head x)) && not (isSpace (head y)) = concateAbsStr (AbsStr (x ++ y) : strs)
  | otherwise = AbsStr x : concateAbsStr (AbsStr y : strs)
concateAbsStr (x : y : strs) = x : concateAbsStr (y:strs)
concateAbsStr strs = strs

splitAbsStr :: [AbsInline] -> [AbsInline] -> [AbsInline]
splitAbsStr _ viewStr = concatMap refine viewStr
  where refine :: AbsInline -> [AbsInline]
        refine (AbsStr str) =
          let (a,b) = break pred str
          in  if null b
                then if null a then [] else [AbsStr a]
                else if pred (head b)
                      then if null a then AbsStr [head b] : refine (AbsStr (tail b)) else AbsStr a : AbsStr [(head b)] : refine (AbsStr (tail b))
                      else error "should not reach here. splitAbsStr."
        refine a = [a]
        pred e = e `elem` CST.Markdown.punctuation

inlineBX :: BiGUL Inline AbsInline
inlineBX =
    Case [ -- Case: AbsStr is a space
         $(normal [| \(Spaces _) (AbsStr s) -> s == " " |] [| \(Spaces _) -> True |])
           ==> Skip (const (AbsStr " "))

         , $(adaptive [| \_ (AbsStr s) -> s == " " |])
           ==> \s v -> (Spaces " ")

           -- Case: AbsStr is a punctuation
         , $(normal [| \(EscapedCharInline _) (AbsStr s) -> length s == 1 && head s `elem` punctuation |]
                    [| \(EscapedCharInline _) -> True |])
           ==> $(update [p| EscapedCharInline c |] [p| AbsStr (c : "") |]
                        [d| c = Replace |])

           -- Case: AbsStr
         , $(normalSV [p| Str _ |] [p| AbsStr _ |] [p| Str _ |])
           ==> $(update [p| Str x |] [p| AbsStr x |] [d| x = Replace |])

         , $(adaptive [| \_ (AbsStr s) -> length s == 1 && head s `elem` punctuation |])
           ==> \s v -> (EscapedCharInline ' ')

         , $(adaptiveSV [p| _ |] [p| AbsStr _ |])
           ==> \_ v -> Str ""

           -- Case: Emph
         , $(normalSV [p| Emph _ |] [p| AbsEmph _ |] [p| Emph _ |])
           ==> $(update [p| Emph x |] [p| AbsEmph x |]
                        [d| x = inlineListBX |])

         , $(adaptiveSV [p| _ |] [p| AbsEmph _ |])
           ==> \_ v -> Emph []

           -- Case: Strong
         , $(normalSV [p| Strong _ |] [p| AbsStrong _ |] [p| Strong _ |])
           ==> $(update [p| Strong x |] [p| AbsStrong x |]
                        [d| x = inlineListBX |])

         , $(adaptiveSV [p| _ |] [p| AbsStrong _ |])
           ==> \_ v -> Strong []

           -- Case: Softbreak
         , $(normalSV [p| Softbreak _ |] [p| AbsSoftbreak |] [p| Softbreak _ |])
           ==> Skip (const AbsSoftbreak)

              -- NOTE: the correct indentation is infered later
         , $(adaptiveSV [p| _ |] [p| AbsSoftbreak |])
           ==> \s v -> Softbreak DefaultIndent

           -- Case: Hardbreak
         , $(normalSV [p| Hardbreak _ _ |] [p| AbsHardbreak |] [p| Hardbreak _ _|])
           ==> Skip (const AbsHardbreak)

         , $(adaptiveSV [p| _ |] [p| AbsHardbreak |])
           ==> \s v -> Hardbreak "  " DefaultIndent

           -- Case: Code
         , $(normalSV [p| InlineCode _ _ |] [p| AbsInlineCode _ |] [p| InlineCode _ _ |])
           ==> $(update [p| InlineCode _ c |] [p| AbsInlineCode c |]
                        [d| c = Replace |])

         , $(adaptiveSV [p| _ |] [p| AbsInlineCode _ |])
           ==> \s v -> (InlineCode "`" "")

           -- Case: AbsLink
         , $(normalSV [p| Link _ _ |] [p| AbsLink _ _ |] [p| Link _ _ |])
           ==> $(update [p| Link t dest |] [p| AbsLink t dest |]
                        [d| t = inlineListBX; dest = Replace |])

         , $(adaptiveSV [p| _ |] [p| AbsLink _ _ |])
           ==> \s v -> (Link [] "")

           -- Case: AbsImage
         , $(normalSV [p| Image _ _ |] [p| AbsImage _ _ |] [p| Image _ _ |])
           ==> $(update [p| Image t dest |] [p| AbsImage t dest |]
                        [d| t = Replace; dest = Replace |])

         , $(adaptiveSV [p| _ |] [p| AbsImage _ _ |])
           ==> \s v -> (Image "" "")
         ]
