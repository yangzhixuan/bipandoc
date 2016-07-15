{-# Language TemplateHaskell, TypeFamilies #-}

module Abstract where

import GHC.Generics
import Generics.BiGUL.TH

data AbsDocument = AbsDocument [AbsBlock]
    deriving (Show, Eq)

data AbsBlock = AbsPara [AbsInline]
              | AbsHeading Int AbsInline -- level, heading
              | AbsUnorderedList [AbsUnorderedListItem]
    deriving (Show, Eq)

data AbsUnorderedListItem = AbsUnorderedListItem AbsInline
    deriving (Show, Eq) 


data AbsInline = AbsStr String
               | AbsSoftbreak
               | AbsHardbreak
               | AbsEmph [AbsInline]
    deriving (Show, Eq)

deriveBiGULGeneric ''AbsDocument
deriveBiGULGeneric ''AbsBlock
deriveBiGULGeneric ''AbsUnorderedListItem
deriveBiGULGeneric ''AbsInline
