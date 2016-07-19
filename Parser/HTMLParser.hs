{-# Language  PatternSynonyms, RecordWildCards, TemplateHaskell, TypeFamilies #-}

module Parser.HTMLParser where

import Text.Megaparsec
import Data.Char (isSpace)
import Control.Monad (liftM, replicateM)
import qualified Data.List.NonEmpty as DLN ( NonEmpty((:|)) )
import Data.Map as Map (fromList, Map, lookup, union, singleton)
import Data.List (groupBy)

import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.TH

import Debug.Trace

data GTree a = GTree a [GTree a] deriving (Show, Eq)

data Document = Document Spaces DocType Spaces HTML Spaces deriving (Show, Eq)
type Spaces = String
type DocType = String
type HTML = GTree CTag

type PU = Parsec Dec String

-- | A single HTML element.
--   There is no requirement for 'TagOpen' and 'TagClose' to match.

data CTag =
    CTag TagMark CTagName [Either Spaces Attribute] CloseMark -- TagName [Spaces | Attributes]
  | CTagText TextMark String         -- ^ A text node, guaranteed not to be the empty string. will not be passed to AST.
  | CTagScript  String               -- ^ A script node
  | CTagComment String               -- ^ A comment
  | CDefaultTag
  deriving (Show, Eq)

data CloseMark = NormalClose | SelfClose | NoClose | NotDecidedCloseMark deriving (Show, Eq)
data TagMark   = Block | Inline | NotDecidedTagMark      deriving (Show, Eq)
data TextMark  = InlineText | OtherText | NotDecidedTextMark deriving (Show, Eq) -- text in block elements such as <p>, <h1>, pass to AST. | text outside block elements which will not be passed to AST.

type CTagName = Either SupportedName OtherName
type OtherName = String

type Attribute = (String, String, String) -- ("src", "   =  ", "\'heheSoManySpaces\'")
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
  | CTable | CTableRow | CTableCell | CTableHeader     -- <table>, <tr>, <td>, <th>
  | CEmph  | CStrong | CStrike          -- <em>, <strong>, <strike>
  | CSuperscript | CSubscript    -- <sup>, <sub>
  | CQuoted         -- <q>
  | CCite           -- <cite>
  | CLink         -- <link>
  | CImg          -- <img>
  deriving (Show, Eq)

  --   CHTML           -- <html>
  -- | CHead           -- <html>
  -- | CBody           -- <html>
  -- | Space        -- do not know what it is. maybe &npsp
  -- | SmallCaps    -- no this
  -- | CEmDash       -- &mdash
  -- | CEnDash       -- &ndash
  -- | CImg
  -- | endnote, footnote... -- no these


supportedName :: Map String SupportedName
supportedName = fromList [
   ("div", CDiv)
  ,("p", CPara)
  ,("code", CCode)
  ,("pre", CPre)
  ,("blockquote", CBlockQuote)
  ,("ol", COrderedList), ("ul", CUnorderedList), ("dl", CDefinitionList), ("li", CListItem)
  ,("h1", CHead 1), ("h2", CHead 2), ("h3", CHead 3), ("h4", CHead 4), ("h5", CHead 5), ("h6", CHead 6)
  ,("hr", CHorizontalRule), ("table", CTable), ("tr", CTableRow), ("td", CTableCell), ("th", CTableHeader)
  ,("em", CEmph), ("strong", CStrong), ("strike", CStrike)
  ,("sup", CSuperscript), ("sub", CSubscript)
  ,("q", CQuoted)
  ,("cite", CCite)
  ,("link", CLink)
  ,("img", CImg)]


deriveBiGULGeneric ''GTree
deriveBiGULGeneric ''Document
deriveBiGULGeneric ''CTag
deriveBiGULGeneric ''SupportedName
deriveBiGULGeneric ''CloseMark
deriveBiGULGeneric ''TagMark
deriveBiGULGeneric ''TextMark


isSupportedName :: Either SupportedName OtherName -> Bool
isSupportedName (Right _) = False
isSupportedName (Left t) = maybe False (const True) (Map.lookup (prtSupportedName t) supportedName)

isSupportedNode :: GTree CTag -> Bool
isSupportedNode (GTree (CTag _ tag _ _) _) = isSupportedName tag
isSupportedNode (GTree (CTagText InlineText _ ) []) = True
isSupportedNode (GTree (CTagText OtherText _ ) []) = False
isSupportedNode (GTree (CTagScript _ ) []) = False
isSupportedNode (GTree (CTagComment _) []) = False
isSupportedNode _ = False


-------------------
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
  (CTag _ tn sOrA _) <- pTagOpen
  inner <- many anyNode
  (string "</html>")
  return $ GTree (CTag Block tn sOrA NormalClose) inner
  <?> "error in parsing html tag"

pTagElement :: PU HTML
pTagElement = do
  posO <-getPosition
  (CTag _ tn sOrA maybeSelfClose) <- try pTagSelfClose <|> pTagOpen
  GTree (CTag _ bb cc dd) ee <-
        case maybeSelfClose of
          SelfClose -> return $ GTree (CTag NotDecidedTagMark tn sOrA SelfClose) []
          NotDecidedCloseMark   ->
            if isVoidElement (prtCTagName tn)
              then do
                iivt <- isIllVoidTag (prtCTagName tn)  -- this is a test function and will not consume input
                if iivt
                  then do
                    inner <- many (try cmtOrTxt) -- maybe some text or comment node...fml
                    --tc    <- try pTagClose
                    try pTagClose
                    return $ GTree (CTag NotDecidedTagMark tn sOrA NormalClose) inner   -- though it's a void element, programmers do not obey the rule and put the ending tag.
                  else return $ GTree (CTag NotDecidedTagMark tn sOrA NoClose) []  -- this is the most expected tag format for void element.

              else do
                inner <- many (try anyNode)
                posC  <- getPosition
                tc    <- pTagClose
                if (prtCTagName tn) == tc
                  then return $ GTree (CTag NotDecidedTagMark tn sOrA NormalClose) inner
                  else error $ "opening and closing tag mismatch. Opening: " ++ wrapTagO (prtCTagName tn) ++ " at " ++ showErrPos posO
                                ++ "\tClosing: " ++ wrapTagC tc ++ " at " ++ showErrPos posC
  let aa = if isInlineTag bb then Inline else Block
  return $ GTree (CTag aa bb cc dd) ee
  <?> "pTagElement"


isIllVoidTag :: String -> PU Bool
isIllVoidTag tn = lookAhead (do
  inner <- many (try cmtOrTxt)              -- illed void tag with some text or comment node...fuck
  tc    <- try pTagClose <|> alwaysSucceeds -- alwaysSucceeds uses lookAhead and thus will not consume input
  return $ if tn == tc
             then True     -- though it's a void element, programmers do not obey the rule and put the ending tag. eg: <br></br>
             else False)   -- this is the most expected tag format for void element. eg: <br>


isInlineTag :: CTagName -> Bool
isInlineTag (Left tag) = isInlineTag' tag
isInlineTag (Right _)  = False

isInlineTag' :: SupportedName -> Bool
isInlineTag' = flip elem [CEmph, CStrong, CStrike, CImg, CLink]

cmtOrTxt :: PU HTML
cmtOrTxt = try pTagComment <|> pTagText

pTagSelfClose :: PU CTag
pTagSelfClose = do
  lab  <- char '<'
  tn   <- pTagName
  sOrA <- many spacesOrAttr
  rab  <- string "/>"
  case Map.lookup tn supportedName of
    Nothing  -> return $ CTag NotDecidedTagMark (Right tn) (mergeSpaces sOrA) SelfClose
    Just tn' -> return $ CTag NotDecidedTagMark (Left tn') (mergeSpaces sOrA) SelfClose
  <?> "pTagSelfClose"


-- TagOpen preSpaces TagName [Either Spaces Attribute]
pTagOpen :: PU CTag
pTagOpen = do
  lab  <- char '<'
  tn   <- pTagName
  sOrA <- many spacesOrAttr
  rab  <- char '>'
  case Map.lookup tn supportedName of
    Nothing  -> return $ CTag NotDecidedTagMark (Right tn) (mergeSpaces sOrA) NotDecidedCloseMark
    Just tn' -> return $ CTag NotDecidedTagMark (Left tn') (mergeSpaces sOrA) NotDecidedCloseMark
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
          return $ GTree (CTagText NotDecidedTextMark text) []
  <?> "pTagText"

pTagComment :: PU HTML
pTagComment = do
  string "<!--"
  s <- lookAhead (replicateM 3 anyChar)
  case s of
    "-->" -> string "-->" >> (return $ GTree (CTagComment "") [])
    _     -> someTill anyChar (string "-->") >>= \cmt -> return $ GTree (CTagComment cmt) []

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
pTagName = some (lowerChar <|> digitChar) <?> "tag name must be lower characters and digits"

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
  CTag _ tn sOrA maybeSelfClose <- try pTagSelfClose <|> pTagOpen
  if (prtCTagName tn) == "script"
  then  case maybeSelfClose of
          SelfClose -> return $ GTree (CTagScript $ "<" ++ (prtCTagName tn) ++ flatSorAs sOrA  ++ "/>") []
          NotDecidedCloseMark -> do
            txt <- manyTill anyChar (string "</script>")
            return $ GTree (CTagScript $ wrapTagO ((prtCTagName tn) ++ flatSorAs sOrA) ++ txt ++ "</script>") []
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

alwaysSucceed :: PU Char
alwaysSucceed = lookAhead anyChar

alwaysSucceeds :: PU String
alwaysSucceeds = liftM (: []) (lookAhead anyChar)

-- void elements
-- these elements has no closing tag. but we permit either <area ... >, <area ... />, or <area ...></area>
isVoidElement :: String -> Bool
isVoidElement = flip elem ["area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"]

------------------------
-- printing function from CST to HTML
prtDocument :: Document -> String
prtDocument (Document pre doctype mid html tra) = pre ++ doctype ++ mid ++ prtHTML html ++ tra

prtHTML :: HTML -> String
prtHTML (GTree (CTag _ tn sOrAs SelfClose) [])     = "<" ++ prtCTagName tn ++ flatSorAs sOrAs ++ "/>"
prtHTML (GTree (CTag _ tn sOrAs NoClose) [])     = "<" ++ prtCTagName tn ++ flatSorAs sOrAs ++ ">"
prtHTML (GTree (CTag _ tn sOrAs NormalClose) children) = wrapTagO (prtCTagName tn ++ flatSorAs sOrAs) ++ concatMap prtHTML children ++ wrapTagC (prtCTagName tn)
prtHTML (GTree (CTagText _ txt) []) = txt
prtHTML (GTree (CTagComment cmt) []) = "<!--" ++ cmt ++ "-->"
prtHTML (GTree (CTagScript txt) []) = txt

flatSorAs :: [Either Spaces Attribute] -> String
flatSorAs [] = ""
flatSorAs (Left sps : xs) = sps ++ flatSorAs xs
flatSorAs (Right (name, eq, val) : xs) = name ++ eq ++ val ++ flatSorAs xs

prtCTagName :: Either SupportedName OtherName -> String
prtCTagName (Left sptdname) = prtSupportedName sptdname
prtCTagName (Right othername) = othername

prtSupportedName :: SupportedName -> String
prtSupportedName CDiv = "div"
prtSupportedName CPara = "p"
prtSupportedName CCode = "code"
prtSupportedName CPre  = "pre"
prtSupportedName CBlockQuote = "blockquote"
prtSupportedName COrderedList    = "ol"
prtSupportedName CUnorderedList  = "ul"
prtSupportedName CDefinitionList = "dl"
prtSupportedName CListItem   = "li"
prtSupportedName (CHead i) = "h" ++ show i
prtSupportedName CHorizontalRule = "hr"
prtSupportedName CTable       = "table"
prtSupportedName CTableRow    = "tr"
prtSupportedName CTableCell   = "td"
prtSupportedName CTableHeader = "th"
prtSupportedName CEmph        = "em"
prtSupportedName CStrong      = "strong"
prtSupportedName CStrike      = "strike"
prtSupportedName CSuperscript = "sup"
prtSupportedName CSubscript   = "sub"
prtSupportedName CQuoted  = "q"
prtSupportedName CCite    = "cite"
prtSupportedName CLink    = "link"
prtSupportedName CImg     = "img"

------------------------
refineDoc :: Document -> Document
refineDoc (Document s0 doctype s1 html s2) = Document s0 doctype s1 (refine1 html) s2

refine1 :: HTML -> HTML
refine1 t@(GTree (CTag mk0 tn attrs mk1) subtags) =
  if isSubtreeInline t
    then GTree (CTag mk0 tn attrs mk1) (concatMap refine2 subtags)
    else GTree (CTag mk0 tn attrs mk1) (map refine1 subtags)
refine1 (GTree (CTagText NotDecidedTextMark txt) []) = GTree (CTagText OtherText txt) []
refine1 t = t

refine2 :: GTree CTag -> [GTree CTag]
refine2 (GTree (CTagText NotDecidedTextMark txt) []) = refine3 txt
refine2 (GTree (CTag Inline tn attrs mk1) subtags) = [(GTree (CTag Inline tn attrs mk1)) (concatMap refine2 subtags)]
refine2 t = error $ "unexpected tag. tag should be text tag or inline tag: " ++ show t

--  groupBy predicate "ab \t\t  defsdfsdf          g"   ---->   ["ab"," \t\t  ","defsdfsdf","          ","g"]
refine3 :: String -> [GTree CTag]
refine3 = map (\txt -> GTree (CTagText InlineText txt) []) . groupBy (\x y -> isSpace x && isSpace y || not (isSpace x) && not (isSpace y))

isSubtreeInline :: GTree CTag -> Bool
isSubtreeInline (GTree (CTag _ (Left tn) _ _) _) = tn `elem` [CPara, CHead 1, CHead 2, CHead 3, CHead 4, CHead 5, CHead 6]
isSubtreeInline _ = False

------------------------
t1 :: IO ()
t1 = do
  i <- readFile "1.html"
  let o  = either (error. show) id (parse parseDoc "1.html" i)
      oo = refineDoc o
  putStrLn "show:"
  putStrLn (show oo)
  putStrLn "print back:"
  putStrLn (prtDocument oo)

t1w :: IO ()
t1w = do
  i <- readFile "1.html"
  let o = either (error. show) show (parse parseDoc "1.html" i)
  writeFile "1o.html" o
  --o <- (liftM (either (error. show) show . (parse parseDoc "1.html")) (readFile "1.html"))
  --writeFile "t1o.html" o

--t2 :: IO ()
--t2 = readFile "t1.html" >>= parseTest parseDoc

