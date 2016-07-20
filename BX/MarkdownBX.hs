{-# Language TemplateHaskell, TypeFamilies #-}
module BX.MarkdownBX where

import Generics.BiGUL
import Data.List
import Data.Char
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH

import BX.BXHelpers

import Abstract
import Parser.Markdown

markdownBX :: BiGUL MarkdownDoc AbsDocument
markdownBX = $(update [p| MarkdownDoc x |] [p| AbsDocument x |]
                      [d| x = blockListBX |])

blockListBX :: BiGUL [Block] [AbsBlock]
blockListBX = (filterLens nonBlankLines) `Compose` (mapLens blockBX createBlock)
    where nonBlankLines (BlankLine _ _) = False
          nonBlankLines _ = True
          createBlock = const $ Para DefaultIndent []

blockBX :: BiGUL Block AbsBlock
blockBX = 
    Case [ -- Case: AbsPara
           $(normalSV [p| Para _ _ |] [p| AbsPara _ |] [p| Para _ _ |])
           ==> $(update [p| Para _ x |] [p| AbsPara x |] [d| x = mapLens inlineBX createInline |])

         , $(adaptiveSV [p| _ |] [p| AbsPara _ |])
           ==> \_ v -> Para DefaultIndent []

           -- Case: AbsHeading
         , $(normalSV [p| ATXHeading _ _ _ _ _ |] [p| AbsHeading _ _ |] [p| ATXHeading _ _ _ _ _ |])
           ==> $(update [p| ATXHeading _ atx _ heading _ |] [p| AbsHeading atx heading |]
                        [d| atx = atxBX; heading = mapLens inlineBX createInline |])

         , $(normal [| \(SetextHeading _ _ _ _ _ _) (AbsHeading level _) -> level == 1 || level == 2|] [| \(SetextHeading _ _ _ _ _ _) -> True |])
           ==> $(update [p| SetextHeading _ heading _ _ setextLine _ |] [p| AbsHeading setextLine heading |]
                        [d| heading = mapLens inlineBX createInline; setextLine = setextLineBX |])

         , $(adaptiveSV [p| _ |] [p| AbsHeading _ _ |])
           ==> \_ v -> ATXHeading DefaultIndent "" " " [] "\n"

           -- Case: AbsUnorderedList
         , $(normalSV [p| UnorderedList _ |] [p| AbsUnorderedList _ |] [p| UnorderedList _ |])
           ==> $(update [p| UnorderedList items |] [p| AbsUnorderedList items |]
                        [d| items = mapLens unorderedListItemBX createListItem |])

         , $(adaptiveSV [p| _ |] [p| AbsUnorderedList _ |])
           ==> \s v -> UnorderedList []

           -- Case: AbsOrderedList
         , $(normalSV [p| OrderedList _ |] [p| AbsOrderedList _ |] [p| OrderedList _ |])
           ==> $(update [p| OrderedList items |] [p| AbsOrderedList items |]
                        [d| items = mapLens orderedListItemBX createListItem |])

         , $(adaptiveSV [p| _ |] [p| AbsOrderedList _ |])
           ==> \s v -> OrderedList []

           -- Case: AbsBlockQuote
         , $(normalSV [p| BlockQuote _ _ _ |] [p| AbsBlockQuote _ |] [p| BlockQuote _ _ _ |])
           ==> $(update [p| BlockQuote _ _ blocks |] [p| AbsBlockQuote blocks |]
                        [d| blocks = blockListBX |])

         , $(adaptiveSV [p| _ |] [p| AbsBlockQuote _ |])
           ==> \s v -> BlockQuote DefaultIndent ">" []

           -- Case: AbsCode
         , $(normalSV [p| IndentedCode _ |] [p| AbsCode _ |] [p| IndentedCode _ |])
           ==> $(update [p| IndentedCode codes |] [p| AbsCode codes |]
                        [d| codes = codeBX |])
         , $(adaptiveSV [p| _ |] [p| AbsCode _ |])
           ==> \s v -> IndentedCode [CodeLine DefaultIndent ""]  -- note that codeBX assume IndentedCode has at least one element
         ]
         where atxBX = emb length (\s v -> replicate v '#')
               setextLineBX = emb (\s -> if head s == '=' then 1 else 2)
                                  (\line level -> replicate (length line) (if level == 1 then '=' else '-'))
               codeBX = emb (concatMap (\(CodeLine ind code) -> code))
                            (\s v -> map (\((CodeLine ind _), vc) -> CodeLine ind vc) 
                                         (zip (s ++ repeat (head s)) (lines v)))
               createInline = const $ Str ""


createListItem :: AbsListItem -> ListItem
createListItem (AbsUnorderedListItem _) = UnorderedListItem DefaultIndent "" '*' " " []
createListItem (AbsOrderedListItem _) = OrderedListItem DefaultIndent "" "1" '.' " " []

unorderedListItemBX :: BiGUL ListItem AbsListItem
unorderedListItemBX = $(update [p| UnorderedListItem _ _ _ _ x |] [p| AbsUnorderedListItem x |] 
                               [d| x = blockListBX |])

orderedListItemBX :: BiGUL ListItem AbsListItem
orderedListItemBX = $(update [p| OrderedListItem _ _ _ _ _ x |] [p| AbsOrderedListItem x |] 
                               [d| x = blockListBX |])


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
           ==> $(update [p| EscapedCharInline c |] [p| AbsStr (c : []) |]
                        [d| c = Replace |])

         , $(adaptive [| \_ (AbsStr s) -> length s == 1 && head s `elem` punctuation |])
           ==> \s v -> (EscapedCharInline ' ')

           -- Case: AbsStr
         , $(normalSV [p| Str _ |] [p| AbsStr _ |] [p| Str _ |])
           ==> $(update [p| Str x |] [p| AbsStr x |] [d| x = Replace |])

         , $(adaptiveSV [p| _ |] [p| AbsStr _ |])
           ==> \_ v -> Str ""

           -- Case: Emph
         , $(normalSV [p| Emph _ |] [p| AbsEmph _ |] [p| Emph _ |])
           ==> $(update [p| Emph x |] [p| AbsEmph x |] 
                        [d| x = mapLens inlineBX createInline |])

         , $(adaptiveSV [p| _ |] [p| AbsEmph _ |])
           ==> \_ v -> Emph []

           -- Case: Strong
         , $(normalSV [p| Strong _ |] [p| AbsStrong _ |] [p| Strong _ |])
           ==> $(update [p| Strong x |] [p| AbsStrong x |] 
                        [d| x = mapLens inlineBX createInline |])

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
                        [d| t = mapLens inlineBX createInline; dest = Replace |])

         , $(adaptiveSV [p| _ |] [p| AbsLink _ _ |])
           ==> \s v -> (Link [] "")

           -- Case: AbsImage
         , $(normalSV [p| Image _ _ |] [p| AbsImage _ _ |] [p| Image _ _ |])
           ==> $(update [p| Image t dest |] [p| AbsImage t dest |]
                        [d| t = Replace; dest = Replace |])

         , $(adaptiveSV [p| _ |] [p| AbsImage _ _ |])
           ==> \s v -> (Image "" "")
         ]
    where createInline = const $ Str ""
