BiPandoc: A Reversible Document Converter with Bidirectional Transformation

BiEditor: Collaborative Document Editing under Different Formats with Bidirectional Transformation
--------------
Research Proposal

###Motivation

**Collaborative editing**: more than one people edit a document at the same time. However there is only one copy of the document, collaborators also have to use the same format (for example, markdown). If we want to enable collaborators to use different formats *at the same time*, a simple idea is to synchronize between formats with a document converter like pandoc.

**Pandoc**: parses documents in some format into an *abstract* structure and renders abstract documents into some concrete format. 

**Weakness of pandoc**: The transformation is not injective (for parsing) or surjective (for rendering). It discards some information when you parses a document into inner representation. So when you use pandoc to convert a document in format A into format B and convert it back (or convert format A into A), the result may be different from the original document. *So it cannot be used for multi-format collaborative editing*. 

**Solution**: we can use **put** action for rendering documents so that the *stylish* information in the original documents can be preserved. In order to ensure that the **put** action and **get** action are consistent, we can use BiGUL. 


###Framework

* (Format 1) \<-ISO-\> (Concrete ST 1) \<S---V\> (Core AST)

* (Format 2) \<-ISO-\> (Concrete ST 2) \<S---V\> (Core AST)

* ...

Explanation:

* Core AST: contains information that will be exchanged between formats
* Concrete ST (CST): isomorphic to plain text format; Besides core information, it also contains *stylish information* (for example, code indentation, meta info, ...)
* Isomorphism between text format and its CST: its consistency can also be checked (e.g. written in BiGUL)

###Plan

1.  Design the CST structure and **(a)** A Markdown CST parser and printer or **(b)** a bidirectional Markdown CST parser.
   
   * ( *Why* we want to implement a parser from scratch? Because markdown's grammar need infinite look-ahead and is quite unspecified. There is no any ready-to-be-used grammar )
   
   * **(a)** Scheme: Write a parser with *parsec* (following the markdown parser of pandoc, or the CommonMark Spec)
   
   * **(b)** Maybe some modification to BiGUL is needed to implementing bi-dir parser. Something like *backtrace* or *continuation*. **NEED** discussion further.
   
2. Markdown CST and AST BX: since the doc is a list of blocks, a key problem is how to match between source and view elements:

    * Node by node matching (like BiYacc): simple to implement, but the corresponding relation can be easily broken by a insertion or a deletion in the list.
    * Matching by *text similarity*: define a matching predict of two block by the similarity of their texts.
    * Matching globally: use a function to calculate how blocks are matched, for example, by maximum matching, A* searching...
        
3. HTML CST parser and BX.


### Evaluation

1. Consistency of transformation *without* modification:     
    1. For a large collection of markdown documents, we transform each document into HTML and transform it back. We verify that every document is identical.
    2. Same experiment with inverse direction (from HTML to markdown).

2. Consistency of transformation *with* modification (Evaluation of our matching strategy):

    For a large collection of markdown documents, and for each document A, we transform it into HTML format H(A), then we modify A into A' and H(A) into H(A)', which the modification should have the same semantics. Then we transform H(A)' back by *put(A, H(A)')* to get A'' and calculate the editing distance between A' and A''. 

    We compare the average editing distance for different matching strategies and pandoc (as a baseline). 
    
### Further Extension

* Smart creation: when we have to create a new source element from a view in the updating phase, we can try to learn the stylish information from the entire source.
* Consider the case that two documents in different format have the same abstract contents (two sources sharing the same view) and if these two documents are both modified, how to synchronize between them. i.e. we need something like `put2_1 :: S1 -> V -> V -> S1` and `put2_2 :: S2 -> V -> V -> S2`.

### Appendix
Pandoc AST:

```haskell
data Pandoc = Pandoc Meta [Block]
              deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
    
-- | Metadata for the document:  title, authors, date.
newtype Meta = Meta { unMeta :: M.Map String MetaValue }
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)
    
data MetaValue = MetaMap (M.Map String MetaValue)
               | MetaList [MetaValue]
               | MetaBool Bool
               | MetaString String
               | MetaInlines [Inline]
               | MetaBlocks [Block]
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)
    
-- | Block element.
data Block
    = Plain [Inline]        -- ^ Plain text, not a paragraph
    | Para [Inline]         -- ^ Paragraph
    | CodeBlock Attr String -- ^ Code block (literal) with attributes
    | RawBlock Format String -- ^ Raw block
    | BlockQuote [Block]    -- ^ Block quote (list of blocks)
    | OrderedList ListAttributes [[Block]] -- ^ Ordered list (attributes
                            -- and a list of items, each a list of blocks)
    | BulletList [[Block]]  -- ^ Bullet list (list of items, each
                            -- a list of blocks)
    | DefinitionList [([Inline],[[Block]])]  -- ^ Definition list
                            -- Each list item is a pair consisting of a
                            -- term (a list of inlines) and one or more
                            -- definitions (each a list of blocks)
    | Header Int Attr [Inline] -- ^ Header - level (integer) and text (inlines)
    | HorizontalRule        -- ^ Horizontal rule
    | Table [Inline] [Alignment] [Double] [TableCell] [[TableCell]]  -- ^ Table,
                            -- with caption, column alignments (required),
                            -- relative column widths (0 = default),
                            -- column headers (each a list of blocks), and
                            -- rows (each a list of lists of blocks)
    | Div Attr [Block]      -- ^ Generic block container with attributes
    | Null                  -- ^ Nothing
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
    
-- | Inline elements.
data Inline
    = Str String            -- ^ Text (string)
    | Emph [Inline]         -- ^ a text (list of inlines)
    | Strong [Inline]       -- ^ Strongly emphasized text (list of inlines)
    | Strikeout [Inline]    -- ^ Strikeout text (list of inlines)
    | Superscript [Inline]  -- ^ Superscripted text (list of inlines)
    | Subscript [Inline]    -- ^ Subscripted text (list of inlines)
    | SmallCaps [Inline]    -- ^ Small caps text (list of inlines)
    | Quoted QuoteType [Inline] -- ^ Quoted text (list of inlines)
    | Cite [Citation]  [Inline] -- ^ Citation (list of inlines)
    | Code Attr String      -- ^ Inline code (literal)
    | Space                 -- ^ Inter-word space
    | SoftBreak             -- ^ Soft line break
    | LineBreak             -- ^ Hard line break
    | Math MathType String  -- ^ TeX math (literal)
    | RawInline Format String -- ^ Raw inline
    | Link Attr [Inline] Target  -- ^ Hyperlink: alt text (list of inlines), target
    | Image Attr [Inline] Target -- ^ Image:  alt text (list of inlines), target
    | Note [Block]          -- ^ Footnote or endnote
    | Span Attr [Inline]    -- ^ Generic inline container with attributes
    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)
```

