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

import Debug.Trace
--------------------------------------------
--------- Concrete Markdown Syntax ---------
--------------------------------------------


data MarkdownDoc = MarkdownDoc [Block]
    deriving (Show, Eq)

data Indent = DefaultIndent | Indent String
    deriving (Show, Eq)

data Block = Para Indent [Inline] -- indent, inlines
           | ATXHeading Indent String String [Inline] String -- indent, atxs, spaces, heading, newline
           | SetextHeading Indent [Inline] String Indent String String -- indent, heading, newline, ind, underline, spaces
           | UnorderedList [ListItem]
           | OrderedList [ListItem]
           | BlockQuote Indent String [Block] -- indent of first block, '>' of first block, blocks
           | BlankLine Indent String -- indent, spaces
           | IndentedCode [CodeLine]
           | FencedCode Indent String String [CodeLine] Indent String String -- indent, fence, spaces, code lines, indent, fence, spaces
    deriving (Show, Eq)

data ListItem = UnorderedListItem Indent String Char String [Block]
                                -- indent, spaces, bullet, spaces, items
              | OrderedListItem Indent String String Char String [Block]
                                -- indent, spaces, number, dot, spaces, items
    deriving (Show, Eq) 

data CodeLine = CodeLine Indent String -- indent, code
    deriving (Show, Eq)


data Inline = Str String
            | Softbreak Indent -- indent
            | Hardbreak String Indent -- spaces, indent
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

defaultMarkdown = ""

putPretty :: Show a => a -> IO ()
putPretty = putStr . ppShow 

printMarkdown :: MarkdownDoc -> String
printMarkdown (MarkdownDoc blks) = concatMap (printBlock "" False) blks

printIndent :: String -> Indent -> String
printIndent defaultIndent DefaultIndent = defaultIndent
printIndent defaultIndent (Indent s) = s

-- For block elements created by BX, we insert a blankline before it
insertBlankLine defaultIndent DefaultIndent = defaultIndent ++ "\n"
insertBlankLine defaultIndent (Indent _) = ""

printBlock :: String -> Bool -> Block -> String
printBlock defaultIndent skipFirstIndent block = case block of
    (BlankLine ind s) -> pInd ind ++ s
    (Para ind inls) -> pInd ind ++ (concatMap (printInline defaultIndent) inls) ++ "\n"
    (ATXHeading ind atxs sps heading sps2) -> pInd ind ++ atxs ++ sps ++ (concatMap (printInline defaultIndent) heading) ++ sps2
    (SetextHeading ind heading sps2 ind2 unls sps) -> pInd ind ++ (concatMap (printInline defaultIndent) heading) ++ sps2 ++ pInd2 ind2 ++ unls ++ sps
    (UnorderedList (it1:items)) -> printListItem defaultIndent skipFirstIndent it1 ++ concatMap (printListItem defaultIndent False) items
    (OrderedList (it1:items)) -> printListItem defaultIndent skipFirstIndent it1 ++ concatMap (printListItem defaultIndent False) items
    (BlockQuote ind str (b1:bs)) -> pInd ind ++ str ++ printBlock (defaultIndent ++ ">") True b1 ++ (concatMap (printBlock (defaultIndent ++ ">") False) bs)
    (IndentedCode codes) -> (if length codes > 0 && (\(CodeLine ind _) -> ind == DefaultIndent) (head codes) then "\n" else "") ++ concatMap (printCodeLine (defaultIndent ++ "    ")) codes
    (FencedCode i1 f1 s1 codes i2 f2 s2) -> (pInd i1) ++ f1 ++ s1 ++ concatMap (printCodeLine defaultIndent) codes ++ pInd2 i2 ++ f2 ++ s2
    where pInd ind = if skipFirstIndent then "" else insertBlankLine defaultIndent ind ++ printIndent defaultIndent ind
          pInd2 ind = printIndent defaultIndent ind

printInline :: String -> Inline -> String
printInline defaultIndent inline = case inline of 
    (Str s) -> s
    (Softbreak ind) -> "\n" ++ pInd ind
    (Hardbreak s ind) -> s ++ "\n" ++ pInd ind
    (Spaces s) -> s
    (Emph inlines) -> "*" ++ (concatMap (printInline defaultIndent) inlines) ++ "*"
    (Strong inlines) -> "**" ++ (concatMap (printInline defaultIndent) inlines) ++ "**"
    (EscapedCharInline c) -> "\\" ++ [c]
    (InlineCode delim codes) -> delim ++ codes ++ delim
    (Link inlines dest) -> "[" ++ (concatMap (printInline defaultIndent) inlines) ++ "]" ++ "(" ++ dest ++ ")"
    (Image alt dest) -> "![" ++ alt ++ "]" ++ "(" ++ dest ++ ")"
    where pInd = printIndent defaultIndent

-- FIXME: split blockquote indentation

printListItem :: String -> Bool -> ListItem -> String
printListItem defaultIndent skipFirstIndent (UnorderedListItem ind sps bullet sps2 (it1:items)) = pInd ind ++ sps ++ [bullet] ++ sps2 ++ printBlock newIndent True it1 ++ concatMap (printBlock newIndent False) items
    where pInd ind = if skipFirstIndent then "" else insertBlankLine defaultIndent ind ++ printIndent defaultIndent ind
          newIndent = defaultIndent ++ " " ++ sps2

printListItem defaultIndent skipFirstIndent (OrderedListItem ind sps number dot sps2 (it1:items)) = pInd ind ++ sps ++ number ++ [dot] ++ sps2 ++ printBlock newIndent True it1 ++ concatMap (printBlock newIndent False) items
    where pInd ind = if skipFirstIndent then "" else insertBlankLine defaultIndent ind ++ printIndent defaultIndent ind
          newIndent = defaultIndent ++ (replicate (length number + 1) ' ') ++ sps2

printCodeLine :: String -> CodeLine -> String
printCodeLine defaultIndent (CodeLine ind code) = printIndent defaultIndent ind ++ code

----------------------------------
--------- Parsers ----------------
----------------------------------

data ParserStatus = ParserStatus { inStrong :: Bool, inEmph :: Bool, inLink :: Bool, indents :: [Indentation], skipIndentOnce :: Bool } deriving (Show)

data Indentation = BlockquoteIndentation | SpaceIndentation Int deriving (Show)

defaultStatus = ParserStatus False False False [] False

markdown ::  Parsec String ParserStatus MarkdownDoc
markdown = do
    blocks <- many block
    eof
    return $ MarkdownDoc blocks

parseMarkdown :: String -> MarkdownDoc
parseMarkdown str = 
    case parseRes of
        Left s -> MarkdownDoc [BlankLine DefaultIndent (show s)]
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
        fencedCode,
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
    -- putState st
    modifyState (\st2 -> st2{indents = indents st})
    s <- spaceChars
    n <- newline
    return $ BlankLine (Indent ind) (s ++ [n])
    where trimLastSpaceIndent = reverse . (dropWhile isSpaceIndent) . reverse
          isSpaceIndent (SpaceIndentation _) = True
          isSpaceIndent _ = False

-- | Parses a atxHeading
atxHeading :: Parsec String ParserStatus Block
atxHeading = try $ do
    ind <- indentation

    atx <- manyRange 1 6 '#'
    spaces <- spaceChars
    heading <- many1 ((notFollowedBy (softbreak <|> hardbreak)) >> inline)
    sps2 <- spaceChars
    newline
    return $ ATXHeading (Indent ind) atx spaces heading (sps2 ++ "\n")

-- | Parses a setextHeading

setextHChars = "=-"

setextHeading :: Parsec String ParserStatus Block
setextHeading = try $ do
    lookAhead $ indentation >> anyLine >> indentation >> many1 (oneOf setextHChars) >> spaceChars >> newline
    ind <- indentation
    heading <- many1 ((notFollowedBy (softbreak <|> hardbreak)) >> inline)
    sps2 <- spaceChars
    newline
    ind2 <- indentation
    chs <- many1 (oneOf setextHChars)
    sp <- spaceChars
    newline
    return $ SetextHeading (Indent ind) heading (sps2 ++ "\n") (Indent ind2) chs (sp ++ "\n")

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
    return $ BlockQuote (Indent ind) (sps ++ ">") blocks
    where addIndent st = (indents st) ++ [BlockquoteIndentation]

-- | Parse an indented code block

indentedCodeLine :: Parsec String ParserStatus CodeLine
indentedCodeLine = try $ do
    ind <- indentation
    sps <- manyRange 4 4 ' '
    code <- anyLine
    return $ CodeLine (Indent (ind ++ sps)) (code ++ "\n")

indentedCode :: Parsec String ParserStatus Block
indentedCode = try $ do
    code <- many1 indentedCodeLine
    return $ IndentedCode code

fencedCodeLine :: Parsec String ParserStatus CodeLine
fencedCodeLine = try $ do
    ind <- indentation
    code <- manyTill anyChar newline
    return $ CodeLine (Indent ind) (code ++ "\n")

fenceChars = "`~"

fencedCode :: Parsec String ParserStatus Block
fencedCode = try $ do
    ind1 <- indentation
    fenceChar <- oneOf fenceChars
    char fenceChar
    char fenceChar
    fenceCharRest <- many (char fenceChar)
    sp1 <- many (satisfy (\c -> c /= '\n'))
    newline
    let fence = fenceChar : fenceChar : fenceChar : fenceCharRest
    code <- many (notFollowedBy (indentation >> string fence >> spaceChars >> newline) >> fencedCodeLine)
    ind2 <- indentation
    fence2 <- string fence
    sp2 <- spaceChars
    newline
    return $ FencedCode (Indent ind1) fence (sp1 ++ "\n") code (Indent ind2) fence2 (sp2 ++ "\n")

-- | Parses a paragraph.
--
-- Paragraph specification:
---- A sequence of non-blank lines that cannot be interpreted as other kinds of blocks forms a paragraph. 
---- The contents of the paragraph are the result of parsing the paragraph’s raw content as inlines. 
---- The paragraph’s raw content is formed by concatenating the lines and removing initial and final whitespace.

paragraph :: Parsec String ParserStatus Block
paragraph = do
    -- rest <- getInput
    -- st <- getState
    -- trace ("p: " ++ (show rest) ++ " with indent: " ++ (show st)) (return ())

    ind <- indentation
    inlines <- many1 inline
    newline
    return $ Para (Indent ind) inlines


-- | Parses a list

unorderedListChar = "-*+"

unorderedListItem :: Parsec String ParserStatus ListItem
unorderedListItem  = try $ do
    st0 <- getState
    ind <- indentation
    sp <- manyRange 0 3 ' '
    ch <- oneOf unorderedListChar
    -- Generally, several spaces is needed between the bullet and the first char of the ListItem.
    -- However, when the first item is a BlankLine, the spaces can be omitted.
    sp2 <- (lookAhead newline >> return "") <|> many1 (char ' ')

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

    return $ UnorderedListItem (Indent ind) sp ch sp2 blocks
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
    sp2 <- (lookAhead newline >> return "") <|> many1 (char ' ')

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

    return $ OrderedListItem (Indent ind) sp num ch sp2 blocks
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
    return $ Softbreak (Indent ind)

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
    st <- getState
    if inEmph st || inStrong st
       then fail "nested emph is not allowed"
       else do
         lookAhead $ char '*' >> notFollowedBy (char '*') >> many1 (notFollowedBy (char '*') >> anyChar) >> char '*'
         char '*'
         modifyState (\st -> st{inEmph = True})
         inlines <- many1 inline
         modifyState (\st -> st{inEmph = False})
         char '*'
         return $ Emph inlines

strong :: Parsec String ParserStatus Inline
strong = try $ do
    st <- getState
    if inStrong st || inEmph st
       then fail "nested strong is not allowed"
       else do
         lookAhead $ string "**" >> many1 (notFollowedBy (string "**") >> anyChar) >> string "**"
         string "**"
         modifyState (\st -> st{inStrong = True})
         inlines <- many1 inline
         modifyState (\st -> st{inStrong = False})
         string "**"
         return $ Strong inlines

escapedCharInline :: Parsec String ParserStatus Inline
escapedCharInline = try $ do
    ch <- escapedChar
    return $ EscapedCharInline ch

punctuation = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

escapedCharSlash :: Parsec String ParserStatus String
escapedCharSlash = try $ do
    char '\\'
    ch <- oneOf punctuation
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
                                             && (not (inStrong st) || c /= '*')
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
