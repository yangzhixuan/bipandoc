{-# Language TemplateHaskell, TypeFamilies #-}

module Markdown where

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Text
import Text.Parsec.Combinator
import GHC.Generics
import Generics.BiGUL.TH

----------------------------------
--------- Concrete Markdown Syntax
----------------------------------

data MarkdownDoc = MarkdownDoc [Block]
    deriving (Show, Eq)


data Block = Para [Inline]
           | ATXHeading String String Inline -- atxs, spaces, heading 
           | SetextHeading Inline String String -- heading, underline, spaces
           | UnorderedList [UnorderedListItem]
           | BlankLine String
    deriving (Show, Eq)

data UnorderedListItem = UnorderedListItem [String] String Char String Inline
                                        -- blanklines, spaces, bullet, spaces, item
    deriving (Show, Eq) 

data Inline = Str String
            | Softbreak
            | Emph [Inline]
    deriving (Show, Eq)

deriveBiGULGeneric ''MarkdownDoc
deriveBiGULGeneric ''Block
deriveBiGULGeneric ''UnorderedListItem
deriveBiGULGeneric ''Inline

----------------------------------
--------- Printers
----------------------------------

printMarkdown :: MarkdownDoc -> String
printMarkdown (MarkdownDoc blks) = concatMap printBlock blks


printBlock :: Block -> String
printBlock (BlankLine s) = s
printBlock (Para inls) = (concatMap printInline inls) ++ "\n"
printBlock (ATXHeading atxs sps heading) = atxs ++ sps ++ (printInline heading) ++ "\n"
printBlock (SetextHeading heading unls sps) = (printInline heading) ++ "\n" ++ unls ++ sps ++ "\n"
printBlock (UnorderedList items) = concatMap printUnorderedListItem items

printInline :: Inline -> String
printInline (Str s) = s
printInline Softbreak = "\n"

printUnorderedListItem :: UnorderedListItem -> String
printUnorderedListItem (UnorderedListItem blks sps bullet sps2 item) = (concat blks) ++ sps ++ [bullet] ++ sps2 ++ (printInline item) ++ "\n"
----------------------------------
--------- Parsers
----------------------------------

markdown ::  Parsec String st MarkdownDoc
markdown = do
    blocks <- many block
    eof
    return $ MarkdownDoc blocks

parseMarkdown :: String -> MarkdownDoc
parseMarkdown str = 
    case parseRes of
        Left _ -> MarkdownDoc [BlankLine ""]
        Right doc -> doc
    where parseRes = parse markdown "" str



----------------------------------
--------- Block Parsers
----------------------------------

block :: Parsec String st Block
block =
    choice [
        blankLine,
        atxHeading,
        setextHeading,
--        orderedList,
        unorderedList,
        paragraph
    ]

-- | Parses a blankline as a block
blankLine :: Parsec String st Block
blankLine = try $ do 
    s <- spaceChars
    n <- newline
    return $ BlankLine (s ++ [n])

-- | Parses a atxHeading
atxHeading :: Parsec String st Block
atxHeading = try $ do
    atx <- manyRange 1 6 '#'
    spaces <- spaceChars
    heading <- inline
    newline
    return $ ATXHeading atx spaces heading

-- | parses a setextHeading

setextHChars = "=-"

setextHeading :: Parsec String st Block
setextHeading = try $ do
    lookAhead $ anyLine >> many1 (oneOf setextHChars) >> spaceChars >> newline
    heading <- inline
    newline
    chs <- many1 (oneOf setextHChars)
    sp <- spaceChars
    newline
    return $ SetextHeading heading chs sp

-- | Parses a paragraph.
--
-- Paragraph specification:
---- A sequence of non-blank lines that cannot be interpreted as other kinds of blocks forms a paragraph. 
---- The contents of the paragraph are the result of parsing the paragraph’s raw content as inlines. 
---- The paragraph’s raw content is formed by concatenating the lines and removing initial and final whitespace.
--
-- Paragraph parsing strategy: 
---- In the parsing phase, lines not parsed by other blocks are simply parsed as ParaLine.
---- After the parsing phase, contiguous ParaLine's are transformed to Para.

-- paraLine :: Parsec String st Block
-- paraLine = try $ do
--     sp <- many (char ' ')
--     -- FIXME the following line used be used when code block is added
--     -- sp <- manyRange 0 3 ' '
--     nsp <- nonspaceChar
--     rest <- anyLine
--     return $ ParaLine $ sp ++ (nsp : rest) ++ "\n"

paragraph :: Parsec String st Block
paragraph = do
    inlines <- many1 multiLineInlines
    newline
    return $ Para inlines


-- | Parses a list
-- We only allow inline-s as list items now

unorderedListChar = "-*+"

unorderedListItem :: Parsec String st UnorderedListItem
unorderedListItem = try $ do
    blanklines <- many (spaceChars >>= \sps -> newline >> (return $ sps ++ "\n"))
    sp <- manyRange 0 3 ' '
    ch <- oneOf unorderedListChar
    sp2 <- many1 (char ' ')
    line <- inline
    newline
    return $ UnorderedListItem blanklines sp ch sp2 line

unorderedList :: Parsec String st Block
unorderedList = try $ do
    items <- many1 unorderedListItem
    return $ UnorderedList items


----------------------------------
--------- Inline Parsers
----------------------------------

-- | Parses an inline
strInline :: Parsec String st Inline
strInline = do 
    chs <- many1 (satisfy $ \c -> c /= '\n')
    return $ Str chs

softbreak :: Parsec String st Inline
softbreak = try $ do
    newline
    lookAhead (spaceChars >> nonspaceChar)
    return $ Softbreak

inline :: Parsec String st Inline
inline = strInline

-- | Parses multi-line inlines
multiLineInlines :: Parsec String st Inline
multiLineInlines = softbreak <|> strInline

----------------------------------
--------- Helper Parsers
----------------------------------

-- | Parses a char specified times
manyRange :: Int -> Int -> Char -> Parsec String st [Char]
manyRange 0 0 ch = return []
manyRange 0 b ch 
    | b > 0 =
        (try $ do
            char ch
            chs <- manyRange 0 (b-1) ch
            return $ ch : chs)
        <|> manyRange 0 0 ch

manyRange a b ch
    | b > a = try $ do
        char ch
        chs <- manyRange (a-1) (b-1) ch
        return $ ch : chs


-- | Parses a space or tab.
spaceChar :: Parsec String st Char
spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

-- | Parses a nonspace, nonnewline character.
nonspaceChar :: Parsec String st Char
nonspaceChar = satisfy $ flip notElem ['\t', ' ', '\n', '\r' ]

-- | zero or more spaces or tabs.
spaceChars :: Parsec String st [Char]
spaceChars = many spaceChar

-- | anyline
anyLine :: Parsec String st String
anyLine = try (manyTill anyChar newline) <|> (many anyChar)
