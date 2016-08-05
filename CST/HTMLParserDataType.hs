{-# LANGUAGE TypeFamilies, TemplateHaskell #-}
module CST.HTMLParserDataType (module CST.HTMLParserDataType, ppShow) where

import Text.Show.Pretty (ppShow)
import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.TH

-- set the data to GTreeLeaf if it !cannot! have any children (eg. text node, void tag)
data GTree a = GTreeNode a [GTree a] | GTreeLeaf a deriving (Show, Eq)


data HTMLDoc = HTMLDoc Spaces DocType Spaces HTML Spaces deriving (Show, Eq)
type Spaces = String
type DocType = String
type HTML = GTree CTag

data CTag =
    CTag TagMark CTagName [Either Spaces Attribute] CloseMark -- TagName [Spaces | Attributes]
  | CTagText TextMark TextContents    -- ^ A text node. Only a text node with TextMark InlineText will be passed to AST.
                                      -- An InlineText node will be further marked as (TM Spaces) or (TR String)
                                      -- But in the parsing stage, we always firstly mark it as TR String.
                                      -- OtherText node will only be distinguished as either (TL Entity) or (TR String).
  | CTagScript  String               -- ^ A script node
  | CTagComment String               -- ^ A comment
  | CCodeContent    String               -- ^ code
  | CDefaultTag
  deriving (Show, Eq)

data CloseMark = NormalClose | SelfClose | NoClose | NotDecidedCloseMark deriving (Show, Eq)
data TagMark   = Block | Inline | NotDecidedTagMark      deriving (Show, Eq)
data TextMark  = InlineText | OtherText | NotDecidedTextMark deriving (Show, Eq) -- text in block elements such as <p>, <h1>, pass to AST. | text outside block elements which will not be passed to AST.

data TextContents = TL Entity | TM Spaces | TR String deriving (Show, Eq)
data Entity = EntitySpace1 | EntitySpace2 | EntityAnd1 | EntityAmp1 | EntityAmp2
            | EntityLT1    | EntityLT2    | EntityGT1  | EntityGT2
            deriving (Show, Eq)
-- EntityXXX1 for &name;   EntityXXX2 for &#number;

type CTagName = Either SupportedName OtherName
type OtherName = String

data Attribute = Attribute String String String deriving (Eq, Show) -- ("src", "   =  ", "\'heheSoManySpaces\'")
type TagName = String
type PreText = String

data SupportedName =
    CDiv            -- <div>
  | CPara           -- <p>
  | CCode           -- <code>
  | CPre            -- <pre>
  | CBlockQuote     -- <blockquote>
  | COrderedList | CUnorderedList | CDefinitionList | CListItem    -- <ol>, <ul>, <dl>, <li>
  | CHead Int -- <h1>, <h2> ... <h6>
  | CHorizontalRule -- <hr>
  | CBr      -- <br>
  | CTable | CTableRow | CTableCell | CTableHeader     -- <table>, <tr>, <td>, <th>
  | CEmph  | CStrong | CStrike          -- <em>, <strong>, <strike>
  | CSuperscript | CSubscript    -- <sup>, <sub>
  | CQuoted         -- <q>
  | CCite           -- <cite>
  | CLink         -- <link>
  | CImg          -- <img>
  deriving (Show, Eq)

deriveBiGULGeneric ''GTree
deriveBiGULGeneric ''HTMLDoc
deriveBiGULGeneric ''CTag
deriveBiGULGeneric ''SupportedName
deriveBiGULGeneric ''CloseMark
deriveBiGULGeneric ''TagMark
deriveBiGULGeneric ''TextMark
deriveBiGULGeneric ''Attribute
deriveBiGULGeneric ''TextContents
deriveBiGULGeneric ''Entity
