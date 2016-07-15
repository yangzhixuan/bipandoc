{-# Language TemplateHaskell, TypeFamilies #-}

module Parser.Markdown where

import Data.Char
import Data.Maybe
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Text
import Text.Parsec.Combinator
import Text.Show.Pretty
import GHC.Generics
import Generics.BiGUL.TH

--------------------------------------------
--------- Concrete Markdown Syntax ---------
--------------------------------------------


data MarkdownDoc = MarkdownDoc [Block]
    deriving (Show, Eq)


data Block = Para String [Inline] -- indent, inlines
           | ATXHeading String String String [Inline] String -- indent, atxs, spaces, heading, newline
           | SetextHeading String [Inline] String String String -- indent, heading, newline, underline, spaces
           | UnorderedList [ListItem]
           | OrderedList [ListItem]
           | BlockQuote String [Block] -- indent, blocks
           | BlankLine String String -- indent, spaces
           | IndentedCode [CodeLine]
    deriving (Show, Eq)

data ListItem = UnorderedListItem String String Char String [Block]
                                -- indent, spaces, bullet, spaces, items
              | OrderedListItem String String String Char String [Block]
                                -- indent, spaces, number, dot, spaces, items
    deriving (Show, Eq) 

data CodeLine = CodeLine String String -- indent, code
    deriving (Show, Eq)


data Inline = Str String
            | Softbreak String -- indent
            | Hardbreak String String -- spaces, indent
            | Spaces String -- spaces
            | Emph [Inline]
            | Strong [Inline]
            | EscapedCharInline Char
            | InlineCode String String -- delim, codes
            | Link [Inline] String -- text, destination
            | Image String String
    deriving (Show, Eq)

deriveBiGULGeneric ''MarkdownDoc
deriveBiGULGeneric ''Block
deriveBiGULGeneric ''ListItem
deriveBiGULGeneric ''Inline

------------------------------------
--------- Printers -----------------
------------------------------------

putPretty :: Show a => a -> IO ()
putPretty = putStr . ppShow 

printMarkdown :: MarkdownDoc -> String
printMarkdown (MarkdownDoc blks) = concatMap printBlock blks

printBlock :: Block -> String
printBlock (BlankLine ind s) = ind ++ s
printBlock (Para ind inls) = ind ++ (concatMap printInline inls) ++ "\n"
printBlock (ATXHeading ind atxs sps heading sps2) = ind ++ atxs ++ sps ++ (concatMap printInline heading) ++ sps2
printBlock (SetextHeading ind heading sps2 unls sps) = ind ++ (concatMap printInline heading) ++ sps2 ++ unls ++ sps
printBlock (UnorderedList items) = concatMap printListItem items
printBlock (OrderedList items) = concatMap printListItem items
printBlock (BlockQuote ind blocks) = ind ++ (concatMap printBlock blocks)
printBlock (IndentedCode codes) = concatMap printCodeLine codes

printInline :: Inline -> String
printInline (Str s) = s
printInline (Hardbreak s ind) = s ++ "\n" ++ ind
printInline (Spaces s) = s
printInline (Softbreak ind) = "\n" ++ ind
printInline (Emph inlines) = "*" ++ (concatMap printInline inlines) ++ "*"
printInline (Strong inlines) = "**" ++ (concatMap printInline inlines) ++ "**"
printInline (EscapedCharInline c) = "\\" ++ [c]
printInline (InlineCode delim codes) = delim ++ codes ++ delim
printInline (Link inlines dest) = "[" ++ (concatMap printInline inlines) ++ "]" ++ "(" ++ dest ++ ")"

printListItem :: ListItem -> String
printListItem (UnorderedListItem ind sps bullet sps2 items) = ind ++ sps ++ [bullet] ++ sps2 ++ (concatMap printBlock items) 
printListItem (OrderedListItem ind sps number dot sps2 items) = ind ++ sps ++ number ++ [dot] ++ sps2 ++ (concatMap printBlock items) 

printCodeLine :: CodeLine -> String
printCodeLine (CodeLine ind code) = ind ++ code

----------------------------------
--------- Parsers ----------------
----------------------------------

data ParserStatus = ParserStatus { inEmph :: Bool, inLink :: Bool, indents :: [Indentation], skipIndentOnce :: Bool }

data Indentation = BlockquoteIndentation | SpaceIndentation Int

defaultStatus = ParserStatus False False [] False

markdown ::  Parsec String ParserStatus MarkdownDoc
markdown = do
    blocks <- many block
    eof
    return $ MarkdownDoc blocks

parseMarkdown :: String -> MarkdownDoc
parseMarkdown str = 
    case parseRes of
        Left _ -> MarkdownDoc [BlankLine "" ""]
        Right doc -> doc
    where parseRes = runParser markdown defaultStatus "" str


----------------------------------
--------- Block Parsers ----------
----------------------------------

block :: Parsec String ParserStatus Block
block =
    choice $ [
        blankLine,
        atxHeading,
        setextHeading,
        orderedList,
        unorderedList,
        blockQuote,
        indentedCode,
        paragraph
    ]

parseIndent BlockquoteIndentation = try $ do
    sp <- manyRange 0 3 ' '
    char '>'
    return $ sp ++ ">"

parseIndent (SpaceIndentation k) = try $ do
    sp <- manyRange k k ' '
    return $ sp

indentation' :: [Indentation] -> Parsec String ParserStatus String
indentation' l = try $ do
    indent <- mapM parseIndent l
    return $ concat indent

indentation :: Parsec String ParserStatus String
indentation = do
    st <- getState
    if skipIndentOnce st 
       then do modifyState (\st -> st{skipIndentOnce = False})
               return ""
       else do ind <- indentation' $ indents st
               return $ ind

-- | Parses a blankline as a block
blankLine :: Parsec String ParserStatus Block
blankLine = try $ do 
    st <- getState
    let newSt = st{indents = trimLastSpaceIndent (indents st)}
    putState newSt
    ind <- indentation
    putState st
    s <- spaceChars
    n <- newline
    return $ BlankLine ind (s ++ [n])
    where trimLastSpaceIndent [] = []
          trimLastSpaceIndent l = 
              case last l of
                  SpaceIndentation _ -> init l
                  _ -> l

-- | Parses a atxHeading
atxHeading :: Parsec String ParserStatus Block
atxHeading = try $ do
    ind <- indentation

    atx <- manyRange 1 6 '#'
    spaces <- spaceChars
    heading <- many1 ((notFollowedBy (spaceChars >> newline)) >> inline)
    sps2 <- spaceChars
    newline
    return $ ATXHeading ind atx spaces heading (sps2 ++ "\n")

-- | Parses a setextHeading

setextHChars = "=-"

setextHeading :: Parsec String ParserStatus Block
setextHeading = try $ do
    lookAhead $ indentation >> anyLine >> indentation >> many1 (oneOf setextHChars) >> spaceChars >> newline
    ind <- indentation
    heading <- many1 ((notFollowedBy (spaceChars >> newline)) >> inline)
    sps2 <- spaceChars
    newline
    indentation
    chs <- many1 (oneOf setextHChars)
    sp <- spaceChars
    newline
    return $ SetextHeading ind heading (sps2 ++ "\n") chs (sp ++ "\n")

-- | Parses a blockquote

blockQuote :: Parsec String ParserStatus Block
blockQuote = try $ do
    ind <- indentation
    sps <- manyRange 0 3 ' '
    char '>'
    st <- getState
    let newIndent = addIndent st
    let newSt1 = st{ indents = newIndent, skipIndentOnce = True }
    putState newSt1
    blocks <- many1 block
    putState st
    return $ BlockQuote (ind ++ sps ++ ">") blocks
    where addIndent st = (indents st) ++ [BlockquoteIndentation]

-- | Parse an indented code block

indentedCodeLine :: Parsec String ParserStatus CodeLine
indentedCodeLine = try $ do
    ind <- indentation
    sps <- manyRange 4 4 ' '
    code <- anyLine
    return $ CodeLine (ind ++ sps) (code ++ "\n")

indentedCode :: Parsec String ParserStatus Block
indentedCode = try $ do
    code <- many1 indentedCodeLine
    return $ IndentedCode code

-- | Parses a paragraph.
--
-- Paragraph specification:
---- A sequence of non-blank lines that cannot be interpreted as other kinds of blocks forms a paragraph. 
---- The contents of the paragraph are the result of parsing the paragraph’s raw content as inlines. 
---- The paragraph’s raw content is formed by concatenating the lines and removing initial and final whitespace.

paragraph :: Parsec String ParserStatus Block
paragraph = do
    ind <- indentation
    inlines <- many1 inline
    newline
    return $ Para ind inlines


-- | Parses a list

unorderedListChar = "-*+"

unorderedListItem :: Parsec String ParserStatus ListItem
unorderedListItem  = try $ do
    ind <- indentation
    sp <- manyRange 0 3 ' '
    ch <- oneOf unorderedListChar
    sp2 <- many1 (char ' ')

    st <- getState
    let newIndent = addIndent st (1 + length sp2)

    -- EXPLANATION: consider:
    -- * abc
    --   def
    -- When we start to parsing abc, the indentation should be skipped, so the indents is empty
    -- but when we start to parsing def, we should parse the indentation

    let newSt = st{ indents = newIndent, skipIndentOnce = True }
    putState newSt
    blocks <- many1 block
    putState st

    return $ UnorderedListItem ind sp ch sp2 blocks
    where addIndent st k = (indents st) ++ [SpaceIndentation k]

unorderedList :: Parsec String ParserStatus Block
unorderedList = try $ do
    items <- many1 unorderedListItem
    return $ UnorderedList items

orderedListItem :: Parsec String ParserStatus ListItem
orderedListItem = try $ do
    ind <- indentation
    sp <- manyRange 0 3 ' '
    num <- many1 (oneOf "1234567890")
    ch <- char '.'
    sp2 <- many1 (char ' ')

    st <- getState
    let newIndent = addIndent st (length num + 1 + length sp2)

    -- EXPLANATION: consider:
    -- 1. abc
    --    def
    -- When we start to parsing abc, the indentation should be skipped, so the indents is empty
    -- but when we start to parsing def, we should parse the indentation.

    let newSt = st{ indents = newIndent, skipIndentOnce = True }
    putState newSt
    blocks <- many1 block
    putState st

    return $ OrderedListItem ind sp num ch sp2 blocks
    where addIndent st k = (indents st) ++ [SpaceIndentation k]

orderedList :: Parsec String ParserStatus Block
orderedList = try $ do
    items <- many1 orderedListItem
    return $ OrderedList items

----------------------------------
--------- Inline Parsers ---------
----------------------------------

-- | Inline parsers

-- Any inline parser should be added to this list, OR strInline may mistakenly override it
specialInlines :: [ Parsec String ParserStatus Inline ]
specialInlines = [softbreak, hardbreak, spaceInline, emph, strong, escapedCharInline, inlineCode, link, image]

softbreak :: Parsec String ParserStatus Inline
softbreak = try $ do
    newline
    ind <- indentation
    lookAhead (spaceChars >> nonspaceChar)
    return $ Softbreak ind

hardbreak :: Parsec String ParserStatus Inline
hardbreak = try $ do
    s1 <- spaceChar
    s2 <- spaceChar
    sps <- spaceChars
    ind <- softbreak >>= \(Softbreak ind) -> return ind
    return $ Hardbreak (s1 : s2 : sps) ind

spaceInline :: Parsec String ParserStatus Inline
spaceInline = try $ do
    notFollowedBy hardbreak
    sps <- many1 spaceChar
    return $ Spaces sps

emph :: Parsec String ParserStatus Inline
emph = try $ do
    lookAhead $ char '*' >> many1 (notBlankline >> notFollowedBy (char '*') >> anyChar) >> char '*'
    char '*'
    st <- getState
    modifyState (\st -> st{inEmph = True})
    inlines <- many1 inline
    putState st
    char '*'
    return $ Emph inlines

strong :: Parsec String ParserStatus Inline
strong = try $ do
    lookAhead $ string "**" >> many (notBlankline >> notFollowedBy (string "**") >> anyChar) >> string "**"
    string "**"
    st <- getState
    modifyState (\st -> st{inEmph = True})
    inlines <- many1 inline
    putState st
    string "**"
    return $ Strong inlines

escapedCharInline :: Parsec String ParserStatus Inline
escapedCharInline = try $ do
    ch <- escapedChar
    return $ EscapedCharInline ch

escapedCharSlash :: Parsec String ParserStatus String
escapedCharSlash = try $ do
    char '\\'
    ch <- oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
    return $ "\\" ++ [ch]

escapedChar :: Parsec String ParserStatus Char
escapedChar = try $ do
    chs <- escapedCharSlash
    return $ head (tail chs)

-- FIXME deal with indentation

inlineCode :: Parsec String ParserStatus Inline
inlineCode = try $ do
    delim <- many1 (char '`')
    let len = length delim
    code <- many1 (notBlankline >> notFollowedBy (string delim) >> anyChar)
    string delim
    return $ InlineCode delim code

-- Simplification for link: no link titles
link :: Parsec String ParserStatus Inline
link = try $ do
    char '['
    st <- getState
    modifyState (\st -> st{inLink = True})
    inlines <- many1 inline
    putState st
    char ']'
    char '('
    dest <- many $ escapedCharSlash <|> ((satisfy $ \c -> c /= '\n' && c /= ')') >>= \x -> return [x])
    char ')'
    return $ Link inlines (concat dest)

-- ![ any text except newline ](url)
image :: Parsec String ParserStatus Inline
image = try $ do
    char '!'
    char '['
    desc <- many $ escapedCharSlash <|> ((satisfy $ \c -> c /= '\n' && c /= ']') >>= \x -> return [x])
    char ']'
    char '('
    dest <- many $ escapedCharSlash <|> ((satisfy $ \c -> c /= '\n' && c /= ')') >>= \x -> return [x])
    char ')'
    return $ Image (concat desc) (concat dest)


-- Any char not in a part of other inlines 
strInline :: Parsec String ParserStatus Inline
strInline = do
    st <- getState
    chs <- many1 $ (notFollowedByAny specialInlines) >>
                    satisfy (\c -> c /= '\n' && (not (inEmph st) || c /= '*')
                                             && (not (inLink st) || c /= ']'))
    return $ Str chs

inline :: Parsec String ParserStatus Inline
inline = choice ( strInline : specialInlines)

----------------------------------
--------- Helper Parsers ---------
----------------------------------

-- | Parses a char specified times
manyRange :: Int -> Int -> Char -> Parsec String ParserStatus [Char]
manyRange 0 0 ch = return []
manyRange 0 b ch 
    | b > 0 =
        (try $ do
            char ch
            chs <- manyRange 0 (b-1) ch
            return $ ch : chs)
        <|> manyRange 0 0 ch

manyRange a b ch
    | b >= a = try $ do
        char ch
        chs <- manyRange (a-1) (b-1) ch
        return $ ch : chs

-- | Parses a space or tab.
spaceChar :: Parsec String ParserStatus Char
spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

-- | Parses a nonspace, nonnewline character.
nonspaceChar :: Parsec String ParserStatus Char
nonspaceChar = satisfy $ flip notElem ['\t', ' ', '\n', '\r' ]

-- | zero or more spaces or tabs.
spaceChars :: Parsec String ParserStatus [Char]
spaceChars = many spaceChar

-- | anyline
anyLine :: Parsec String ParserStatus String
anyLine = try (manyTill anyChar newline) <|> (many anyChar)

-- notFollowedByAny
notFollowedByAny [] = return $ ()
notFollowedByAny (x:xs) = do
    notFollowedBy x
    notFollowedByAny xs
    return $ ()

-- | The following line is not a blank line
notBlankline = notFollowedBy (newline >> spaceChars >> newline)
