{-# Language TemplateHaskell, TypeFamilies #-}

module Abstract where

import GHC.Generics
import Generics.BiGUL.TH

data AbsDocument = AbsDocument [AbsBlock]
    deriving (Show, Eq)

data AbsBlock = AbsPara [AbsInline]
              | AbsHeading Int [AbsInline] -- level, heading
              | AbsUnorderedList [AbsListItem]
              | AbsOrderedList [AbsListItem]
              | AbsBlockQuote [AbsBlock]
              | AbsCode String
    deriving (Show, Eq)

data AbsListItem = AbsUnorderedListItem [AbsBlock]
                 | AbsOrderedListItem [AbsBlock]
    deriving (Show, Eq) 

data AbsInline = AbsStr String
               | AbsSoftbreak
               | AbsHardbreak
               | AbsEmph [AbsInline]
               | AbsStrong [AbsInline]
               | AbsInlineCode String
               | AbsLink [AbsInline] String -- text, target
               | AbsImage String String -- alt, target
    deriving (Show, Eq)

deriveBiGULGeneric ''AbsDocument
deriveBiGULGeneric ''AbsBlock
deriveBiGULGeneric ''AbsListItem
deriveBiGULGeneric ''AbsInline
