{-# Language TemplateHaskell, TypeFamilies #-}
module BX.MarkdownBX where

import Generics.BiGUL
import Data.List
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH

import BX.BXHelpers

import Abstract
import Parser.Markdown

markdownBX :: BiGUL MarkdownDoc AbsDocument
markdownBX = $(update [p| MarkdownDoc x |] [p| AbsDocument x |]
                      [d| x = (filterLens nonBlankLines) `Compose` (mapLens blockBX createBlock) |])
    where createBlock = const $ BlankLine ""
          nonBlankLines (BlankLine _) = False
          nonBlankLines _ = True

createInline :: AbsInline -> Inline
createInline (AbsStr s) = Str s
createInline (AbsEmph ins) = Emph (map createInline ins)


inlineBX :: BiGUL Inline AbsInline
inlineBX = 
    Case [ $(normalSV [p| Str _ |] [p| AbsStr _ |] [p| Str _ |])
           ==> $(update [p| Str x |] [p| AbsStr x |] [d| x = Replace |])

         , $(adaptiveSV [p| _ |] [p| AbsStr _ |])
           ==> \_ v -> Str ""

         , $(normalSV [p| Emph _ |] [p| AbsEmph _ |] [p| Emph _ |])
           ==> $(update [p| Emph x |] [p| AbsEmph x |] 
                        [d| x = mapLens inlineBX createInline |])

         , $(adaptiveSV [p| _ |] [p| AbsEmph _ |])
           ==> \_ v -> Emph []

         , $(normalSV [p| Softbreak |] [p| AbsSoftbreak |] [p| Softbreak |])
           ==> Skip (const AbsSoftbreak)

         , $(adaptiveSV [p| _ |] [p| AbsSoftbreak |])
           ==> \s v -> Softbreak
         ]

createUnorderedItem :: AbsUnorderedListItem -> UnorderedListItem
createUnorderedItem (AbsUnorderedListItem absInline) = UnorderedListItem [] "" '*' " " (createInline absInline)

unorderedListItemBX :: BiGUL UnorderedListItem AbsUnorderedListItem
unorderedListItemBX = $(update [p| UnorderedListItem _ _ _ _ x |] [p| AbsUnorderedListItem x |] [d| x = inlineBX |])

blockBX :: BiGUL Block AbsBlock
blockBX = 
    Case [ -- Case: AbsPara
           $(normalSV [p| Para _ |] [p| AbsPara _ |] [p| Para _ |])
           ==> $(update [p| Para x |] [p| AbsPara x |] [d| x = mapLens inlineBX createInline |])

         , $(adaptiveSV [p| _ |] [p| AbsPara _ |])
           ==> \_ v -> Para []

           -- Case: AbsHeading
         , $(normalSV [p| ATXHeading _ _ _ |] [p| AbsHeading _ _ |] [p| ATXHeading _ _ _ |])
           ==> $(update [p| ATXHeading atx _ heading |] [p| AbsHeading atx heading |]
                        [d| atx = atxBX; heading = inlineBX |])

         , $(normal [| \(SetextHeading _ _ _) (AbsHeading level _) -> level == 1 || level == 2|] [| \(SetextHeading _ _ _) -> True |])
           ==> $(update [p| SetextHeading heading setextLine _ |] [p| AbsHeading setextLine heading |]
                        [d| heading = inlineBX; setextLine = setextLineBX |])

         , $(adaptiveSV [p| _ |] [p| AbsHeading _ _ |])
           ==> \_ v -> ATXHeading "" "" (Str "")

           -- Case: AbsUnorderedList
         , $(normalSV [p| UnorderedList _ |] [p| AbsUnorderedList _ |] [p| UnorderedList _ |])
           ==> $(update [p| UnorderedList items |] [p| AbsUnorderedList items |]
                        [d| items = mapLens unorderedListItemBX createUnorderedItem |])

         ]
         where atxBX = emb length (\s v -> replicate v '#')
               setextLineBX = emb (\s -> if head s == '=' then 1 else 2)
                                  (\line level -> replicate (length line) (if level == 1 then '=' else '-'))
