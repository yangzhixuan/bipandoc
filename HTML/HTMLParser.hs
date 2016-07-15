{-# Language  ViewPatterns, RecordWildCards #-}

-- 唉我tm好迷茫啊。做文档转换，是不是不用考虑input，frame这些乱七八糟的和展示文本没关系的构件？
-- 反正pandoc是直接过滤掉了这些东西。。。只保留了p，ul，head这些个最基本的用于渲染文本的标签。
-- 但是为了勉为其难的满足GetPut和PutGet，这些东西又不应该（不能）删。
-- 讲道理HTML parse出来的CST肯定是一棵类似general tree的树。那么AST如果要求比CST简单，还能长啥样呢？
-- 好像扔掉啥都不行，因为HTML里面基本上没有空白这一说法。除了<img   src  =   "" ...>这种tag里面的空格可以省略，
-- 其他都不行。两个tag之间的所有信息都是text node或者comment node，能扔个jb。。。
-- 另一个思路是仿照pandoc的AST，细分出了block，paragraph，list。。。这样AST比CST更复杂，不知道该不该叫AST
-- 而且感觉用BiGUL写这个 general tree <-> AST的转换麻烦的要死，还不如直接给CST弄成有这种细分后的结构的树。
-- 然后CST到AST就是个简单的filter，给input，frame之类的乱七八糟的在其他文档格式中不可能出现的元素直接过滤掉。
-- 既（可能）能满足getput，putget，又不复杂，还用到了bigul，多好哟。说实在的这么做挺无聊的。。。
-- 还是（future work里）解决多人编辑问题有意义

module HTMLParser where

import Text.Megaparsec
import Data.Char (isSpace)
import Control.Monad (liftM, replicateM)
import qualified Data.List.NonEmpty as DLN ( NonEmpty((:|)) )

import Debug.Trace

data GTree a = GTree a [GTree a] deriving (Eq, Ord)

data Document = Document Spaces DocType Spaces HTML Spaces deriving (Eq, Ord)
type Spaces = String
type DocType = String
type HTML = GTree Tag

type PU = Parsec Dec String

-- | A single HTML element.
--   There is no requirement for 'TagOpen' and 'TagClose' to match.
data Tag =
     Tag     TagName [Either Spaces Attribute] String  -- TagName [Spaces | Attributes] CloseTag|SelfClose
   | TagVoid TagName [Either Spaces Attribute]         -- void tag without (self-) closing tag. eg: <br> <meta ...>
   | TagText String                  -- ^ A text node, guaranteed not to be the empty string
   | TagScript String                -- ^ A script node
   | TagComment String               -- ^ A comment
     deriving (Show, Eq, Ord)

type Attribute = (String, String, String) -- ("src", "   =  ", "\'heheSoManySpaces\'")
type TagName = String
type PreText = String

parseDoc :: Parsec Dec String Document
parseDoc = do
  space0 <- spaceChars
  d      <- pDocType
  space1 <- spaceChars
  html   <- pHTML
  space2 <- spaceChars <?> "trailing spaces at end of the html file"
  eof
  return $ Document space0 d space1 html space2
  <?> "parseDoc"

anyNode :: PU HTML
anyNode = try pScriptTag <|>  try pTagElement <|> try pTagComment <|> pTagText <?> "anyNode"

pHTML :: PU HTML
pHTML = do
  (Tag tn sOrA _) <- pTagOpen
  inner <- many anyNode
  tc    <- endHTML
  return $ GTree (Tag tn sOrA tc) inner
  <?> "pHTML"

endHTML :: PU String
endHTML = (string "</html>") >> return "html"


pTagElement :: PU HTML
pTagElement = do
  posO <-getPosition
  (Tag tn sOrA maybeSelfClose) <- try pTagSelfClose <|> pTagOpen
  case maybeSelfClose of
    "/>" -> return $ GTree (Tag tn sOrA maybeSelfClose) []
    ""   ->
      if isVoidElement tn
        then do
          iivt <- isIllVoidTag tn  -- this is a test function and will not consume input
          if iivt
            then do
              inner <- many (try cmtOrTxt) -- maybe some text or comment node...fml
              tc    <- try pTagClose
              return $ GTree (Tag tn sOrA tc) inner   -- though it's a void element, programmers do not obey the rule and put the ending tag.
            else return $ GTree (TagVoid tn sOrA) []  -- this is the most expected tag format for void element.

        else do
          inner <- many (try anyNode)
          posC  <- getPosition
          tc    <- pTagClose
          if tn == tc
            then return $ GTree (Tag tn sOrA tc) inner
            else error $ "opening and closing tag mismatch. Opening: " ++ wrapTagO tn ++ " at " ++ showErrPos posO
                          ++ "\tClosing: " ++ wrapTagC tc ++ " at " ++ showErrPos posC
  <?> "pTagElement"


isIllVoidTag :: String -> PU Bool
isIllVoidTag tn = lookAhead (do
  inner <- many (try cmtOrTxt)              -- illed void tag with some text or comment node...fuck
  tc    <- try pTagClose <|> alwaysSucceeds -- alwaysSucceeds uses lookAhead and thus will not consume input
  return $ if tn == tc
             then True     -- though it's a void element, programmers do not obey the rule and put the ending tag. eg: <br></br>
             else False)   -- this is the most expected tag format for void element. eg: <br>

cmtOrTxt :: PU HTML
cmtOrTxt = try pTagComment <|> pTagText

pTagSelfClose :: PU Tag
pTagSelfClose = do
  lab  <- char '<'
  tn   <- pTagName
  sOrA <- many spacesOrAttr
  rab  <- string "/>"
  return $ Tag tn (mergeSpaces sOrA) "/>"
  <?> "pTagSelfClose"


-- TagOpen preSpaces TagName [Either Spaces Attribute]
pTagOpen :: PU Tag
pTagOpen = do
  lab  <- char '<'
  tn   <- pTagName
  sOrA <- many spacesOrAttr
  rab  <- char '>'
  return $ Tag tn (mergeSpaces sOrA) ""
  <?> "pTagOpen"

pTagClose :: PU String
pTagClose = do {string "</"; tn <- pTagName; char '>'; return $ tn} <?> "pTagClose"

trimCloseTag :: String -> String
trimCloseTag x = drop 2 . take (length x - 1) $ x

pTagText :: PU HTML
pTagText = do
  c <- lookAhead (anyChar)
  case c of
    '<' -> unexpected (Label ((DLN.:|) '<' []) )
    _   -> do
          text <- someTill anyChar (lookAhead (string "<"))
          return $ GTree (TagText text) []
  <?> "pTagText"

pTagComment :: PU HTML
pTagComment = do
  string "<!--"
  s <- lookAhead (replicateM 3 anyChar)
  case s of
    "-->" -> string "-->" >> (return $ GTree (TagComment "") [])
    _     -> someTill anyChar (string "-->") >>= \cmt -> return $ GTree (TagComment cmt) []

pDocType :: PU DocType
pDocType = do
  pre <- spaceChars
  o   <- string "<!"
  d   <-string' "doctype"
  mid <- spaceChars
  whatever <- someTill anyChar (string ">")
  return $ pre ++ o ++ d ++ mid ++ whatever ++ ">"
  <?> "pDocType"

spacesOrAttr :: PU (Either Spaces Attribute)
spacesOrAttr = (spaceChar >>= return . Left . flip (:) []) <|> (pAttribute >>= return . Right)

pTagName :: PU String
pTagName = some lowerChar <?> "tag name must be lower characters"

-- type Attribute = (String, String, String)        ("src", "   =  ", "\'heheSoManySpaces\'")
pAttribute :: PU Attribute
pAttribute = do
  an <- some (lowerChar <|> char '-' <?> "attribute name must be lower characters or hyphen")
  s0 <- spaceChars
  char '='
  s1 <- spaceChars
  av <- attrInQuote <|> attrInDQuote <?> "Attribute should be put in quotes"
  return (an, s0 ++ "=" ++ s1, av)
  <?> "pAttribute"
  where
    attrInQuote :: PU String
    attrInQuote = do
      char '\''
      av <- manyTill anyChar (lookAhead (string "'"))
      char '\''
      return $ "'" ++ av ++ "'"
    attrInDQuote :: PU String
    attrInDQuote = do
      char '"'
      av <- manyTill anyChar (lookAhead (string "\""))
      char '"'
      return $ "\"" ++ av ++ "\""


pScriptTag :: PU HTML
pScriptTag = do
  Tag tn sOrA maybeSelfClose <- pTagOpen
  if tn == "script"
  then  if maybeSelfClose == "/>"
        then return $ GTree (TagScript $ "<" ++ tn ++ flatSorAs sOrA  ++ "/>") []
        else do
          txt <- manyTill anyChar (string "</script>")
          let gt = GTree (TagScript $ wrapTagO (tn ++ flatSorAs sOrA) ++ txt ++ "</script>") []
          --traceM:r (show gt)
          return gt
  else unexpected (Label ((DLN.:|) 's' []) )

spaceChars :: PU String
spaceChars = many spaceChar <?> "spaceChars"


mergeSpaces :: [Either Spaces Attribute] -> [Either Spaces Attribute]
mergeSpaces (Left s : Left " " : xs) = mergeSpaces $ Left (s ++ " ") : xs
mergeSpaces (s : Right a : xs)  = s : Right a : mergeSpaces xs
mergeSpaces ([x]) = [x]
mergeSpaces []    = []


showErrPos :: SourcePos -> String
showErrPos (SourcePos {..}) = "(line " ++ drop 4 (show sourceLine) ++ ", column " ++ drop 4 (show sourceColumn) ++ ")"

wrapTagO :: String -> String
wrapTagO tag = "<" ++ tag ++ ">"

wrapTagC :: String -> String
wrapTagC tag = "</" ++ tag ++ ">"


-- printing function from CST to HTML
instance Show Document where
  show (Document pre doctype mid html tra) = pre ++ doctype ++ mid ++ show html ++ tra

instance Show (GTree Tag) where
  show (GTree (Tag tn sOrAs "/>") [])     = "<" ++ tn ++ flatSorAs sOrAs ++ "/>"
  show (GTree (Tag tn sOrAs tc) children) = wrapTagO (tn ++ flatSorAs sOrAs) ++ concatMap show children ++ wrapTagC tc
  show (GTree (TagText txt) []) = txt
  show (GTree (TagComment cmt) []) = "<!--" ++ cmt ++ "-->"
  show (GTree (TagScript txt) []) = txt
  show (GTree (TagVoid tn sOrA) []) = wrapTagO (tn ++ flatSorAs sOrA)


flatSorAs :: [Either Spaces Attribute] -> String
flatSorAs [] = ""
flatSorAs (Left sps : xs) = sps ++ flatSorAs xs
flatSorAs (Right (name, eq, val) : xs) = name ++ eq ++ val ++ flatSorAs xs
--------------------

alwaysSucceed :: PU Char
alwaysSucceed = lookAhead anyChar

alwaysSucceeds :: PU String
alwaysSucceeds = liftM (: []) (lookAhead anyChar)

-- void elements
-- these elements has no closing tag. but we permit either <area ... >, <area ... />, or <area ...></area>
isVoidElement :: String -> Bool
isVoidElement = flip elem ["area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"]



-----------------------
-- transformation for refining CST datatypes

--data CHTML = CHTML CHead CBody deriving (Eq, Ord, Read)

--type CHead = String -- assume head will not be showed or edited in other document format
--data CBody = CBody CBodyTag [CBlock] deriving (Eq, Ord)


--data CTag =
--     CTag  CTagName [Either Spaces Attribute] String  -- TagName [Spaces | Attributes] CloseTag|SelfClose
--   | CTagVoid TagName [Either Spaces Attribute]         -- void tag without (self-) closing tag. eg: <br> <meta ...>
--   | CTagText String                  -- ^ A text node, guaranteed not to be the empty string
--   | CTagScript String                -- ^ A script node
--   | CTagComment String               -- ^ A comment
--   | CTagBlock CBlock
--     deriving (Show, Eq, Ord)


data CTagName =
    CDiv   -- <div>
  | CPara  -- <p>
  | CCode  -- <code>
  | CPre   -- <pre>
  | CBlockQuote -- <blockquote>
  | COrderedList    -- <ol>
  | CUnorderedList  -- <ul>
  | CDefinitionList -- <dl>
  | CHeader1        -- <h1>
  | CHeader2
  | CHeader3
  | CHeader4
  | CHeader5
  | CHeader6
  | CHorizontalRule -- <hr>
  | CTable          -- <table>
  | CTableRow       -- <tr>
  | CTableCell      -- <td>
  | CTableHeader    -- <th>
  deriving (Eq, Enum)

-- | Block element.
--data CBlock
--    = CPlain [CInline]        -- ^ Plain text, not a paragraph
--    | CPara  [CInline]         -- ^ Paragraph
--    | CodeBlock Attr String -- ^ Code block (literal) with attributes
--    | RawBlock Format String -- ^ Raw block
--    | BlockQuote [Block]    -- ^ Block quote (list of blocks)
--    | OrderedList ListAttributes [[Block]] -- ^ Ordered list (attributes
--                            -- and a list of items, each a list of blocks)
--    | BulletList [[Block]]  -- ^ Bullet list (list of items, each
--                            -- a list of blocks)
--    | DefinitionList [([Inline],[[Block]])]  -- ^ Definition list
--                            -- Each list item is a pair consisting of a
--                            -- term (a list of inlines) and one or more
--                            -- definitions (each a list of blocks)
--    | Header Int Attr [Inline] -- ^ Header - level (integer) and text (inlines)
--    | HorizontalRule        -- ^ Horizontal rule
--    | Table [Inline] [Alignment] [Double] [TableCell] [[TableCell]]  -- ^ Table,
--                            -- with caption, column alignments (required),
--                            -- relative column widths (0 = default),
--                            -- column headers (each a list of blocks), and
--                            -- rows (each a list of lists of blocks)
--    | Div Attr [Block]      -- ^ Generic block container with attributes
--    | Null                  -- ^ Nothing
--    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)


--data Inline
--    = Str String            -- ^ Text (string)
--    | Emph [Inline]         -- ^ Emphasized text (list of inlines)
--    | Strong [Inline]       -- ^ Strongly emphasized text (list of inlines)
--    | Strikeout [Inline]    -- ^ Strikeout text (list of inlines)
--    | Superscript [Inline]  -- ^ Superscripted text (list of inlines)
--    | Subscript [Inline]    -- ^ Subscripted text (list of inlines)
--    | SmallCaps [Inline]    -- ^ Small caps text (list of inlines)
--    | Quoted QuoteType [Inline] -- ^ Quoted text (list of inlines)
--    | Cite [Citation]  [Inline] -- ^ Citation (list of inlines)
--    | Code String           -- ^ Inline code (literal)
--    | Space                 -- ^ Inter-word space
--    | EmDash                -- ^ Em dash
--    | EnDash                -- ^ En dash
--    | Apostrophe            -- ^ Apostrophe
--    | Ellipses              -- ^ Ellipses
--    | LineBreak             -- ^ Hard line break
--    | Math MathType String  -- ^ TeX math (literal)
--    | TeX String            -- ^ LaTeX code (literal)
--    | HtmlInline String     -- ^ HTML code (literal)
--    | Link [Inline] Target  -- ^ Hyperlink: text (list of inlines), target
--    | Image [Inline] Target -- ^ Image:  alt text (list of inlines), target
--                            -- and target
--    | Note [Block]          -- ^ Footnote or endnote
--    deriving (Show, Eq, Ord, Read, Typeable, Data)


t1 :: IO ()
t1 = readFile "1.html" >>= parseTest parseDoc

t1w :: IO ()
t1w = do
  i <- readFile "1.html"
  let o = either (error. show) show (parse parseDoc "1.html" i)
  writeFile "1o.html" o
  --o <- (liftM (either (error. show) show . (parse parseDoc "1.html")) (readFile "1.html"))
  --writeFile "t1o.html" o

--t2 :: IO ()
--t2 = readFile "t1.html" >>= parseTest parseDoc

