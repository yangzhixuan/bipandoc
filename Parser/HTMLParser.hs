{-# Language RecordWildCards, TemplateHaskell, TypeFamilies #-}

-- todo: keep spaces and linebreaks in <pre>. treat them as what they are

module Parser.HTMLParser (parseHTML, prtDocument, prtDocumentBody, isSupportedNode, emptyHTMLCST, emptyHTMLStr) where

import Parser.HTMLParserDataType
import Text.Megaparsec
import Data.Char (isSpace)
import Data.List (groupBy)
import Data.Maybe (fromJust, fromMaybe)
import Control.Monad (liftM, replicateM)
import qualified Data.List.NonEmpty as DLN ( NonEmpty((:|)), fromList )
import Data.Map as Map (fromList, Map, lookup, union, singleton, keys, findWithDefault)
import Text.Show.Pretty (ppShow)

import Control.Monad.Writer
import qualified Data.Set as Set (fromList, singleton, empty)

import Debug.Trace

type PU = Parsec Dec String

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
  ,("br", CBr)
  ,("sup", CSuperscript), ("sub", CSubscript)
  ,("q", CQuoted)
  ,("cite", CCite)
  ,("a", CLink)
  ,("img", CImg)]


isSupportedName :: Either SupportedName OtherName -> Bool
isSupportedName (Right "body") = True -- an ad hoc trick for the filter lens on gtree.
isSupportedName (Right _) = False
isSupportedName (Left t) = maybe False (const True) (Map.lookup (prtSupportedName t) supportedName)

isSupportedNode :: GTree CTag -> Bool
isSupportedNode (GTreeNode (CTag _ tag _ _) _) = isSupportedName tag
isSupportedNode (GTreeLeaf (CTag _ tag _ _)) = isSupportedName tag
isSupportedNode (GTreeLeaf (CTagText InlineText _)) = True
isSupportedNode (GTreeLeaf (CCodeContent _)) = True
isSupportedNode (GTreeLeaf (CTagText OtherText _)) = False
isSupportedNode (GTreeLeaf (CTagScript _ )) = False
isSupportedNode (GTreeLeaf (CTagComment _)) = False
isSupportedNode n = error $ "node not supported: " ++ show n


-------------html parser
parseDoc :: Parsec Dec String HTMLDoc
parseDoc = do
  space0 <- spaceChars
  d      <- pDocType
  space1 <- spaceChars
  html   <- pHTMLNode
  space2 <- spaceChars <?> "trailing spaces at end of the html file"
  eof
  return $ HTMLDoc space0 d space1 html space2
  <?> "parseDoc"

pDocType :: PU DocType
pDocType = do
  pre <- spaceChars
  o   <- string "<!"
  d   <-string' "doctype"
  mid <- spaceChars
  whatever <- someTill anyChar (string ">")
  return $ pre ++ o ++ d ++ mid ++ whatever ++ ">"
  <?> "pDocType"


pHTMLNode :: PU HTML
pHTMLNode = do
  CTag _ tn sOrA _ <- pTagOpen
  inner <- many pAnyNode <?> "\tparse <head> or <body> fail. maybe they are not closed properly?"
  string "</html>" <?> "</html>, is <html> closed properly?"
  return $ GTreeNode (CTag Block tn sOrA NormalClose) inner
  <?> "error in parsing html tag"

pTagElement :: PU HTML
pTagElement = do
  posO <-getPosition
  CTag _ tn sOrA maybeSelfClose <- pTagSelfCloseOrOpen
  GTreeNode (CTag _ bb cc dd) ee <-
    case maybeSelfClose of
      SelfClose -> return $ GTreeNode (CTag undefined tn sOrA SelfClose) [] -- set to GTreeNode for easier handling. changed to GTreeLeaf later in this function
      NotDecidedCloseMark   ->
        if isVoidElement (prtCTagName tn)
          then return $ GTreeNode (CTag undefined tn sOrA NoClose) []  -- for void element.
          else do
            inner <- many pAnyNode
            posC  <- getPosition
            tc    <- pTagClose
            if (prtCTagName tn) == tc
              then return $ GTreeNode (CTag undefined tn sOrA NormalClose) inner
              else error $ "opening and closing tag mismatch. Opening: " ++ wrapTagO (prtCTagName tn) ++ " at " ++ showErrPos posO
                            ++ "\tClosing: " ++ wrapTagC tc ++ " at " ++ showErrPos posC
  let aa = if isInlineTag bb then Inline else Block
  return $ if isVoidElement (prtCTagName bb) then GTreeLeaf (CTag aa bb cc dd) else GTreeNode (CTag aa bb cc dd) ee
  <?> "pTagElement"

pAnyNode :: PU HTML
pAnyNode = try pScriptTag <|> try pTagCode <|> try pTagElement <|> try pTagComment <|> pTagText <?> "parse tags fail. maybe some tags are not closed properly"

isInlineTag :: CTagName -> Bool
isInlineTag (Left tag) = isInlineTag' tag
isInlineTag (Right _)  = False

isInlineTag' :: SupportedName -> Bool
isInlineTag' = flip elem [CEmph, CStrong, CStrike, CImg, CLink, CBr]

pTagSelfCloseOrOpen :: PU CTag
pTagSelfCloseOrOpen = do
  char '<'
  tn   <- pTagName
  sOrA <- many (spacesOrAttr <?> "parse attribute error")
  ((   string "/>" >>
       case Map.lookup tn supportedName of
         Nothing  -> return $ CTag NotDecidedTagMark (Right tn) (mergeSpaces sOrA) SelfClose
         Just tn' -> return $ CTag NotDecidedTagMark (Left tn') (mergeSpaces sOrA) SelfClose) <|>
   (   char '>' >>
       case Map.lookup tn supportedName of
         Nothing  -> return $ CTag NotDecidedTagMark (Right tn) (mergeSpaces sOrA) NotDecidedCloseMark
         Just tn' -> return $ CTag NotDecidedTagMark (Left tn') (mergeSpaces sOrA) NotDecidedCloseMark))
  <?> "ptag selfclose or open"


-- pTagSelfClose :: PU CTag
-- pTagSelfClose = do
--   char '<'
--   tn   <- pTagName
--   sOrA <- many spacesOrAttr <?> "parse attribute error"
--   string "/>"
--   case Map.lookup tn supportedName of
--     Nothing  -> return $ CTag NotDecidedTagMark (Right tn) (mergeSpaces sOrA) SelfClose
--     Just tn' -> return $ CTag NotDecidedTagMark (Left tn') (mergeSpaces sOrA) SelfClose
--   <?> "pTagSelfClose"
-- 
-- 
-- TagOpen preSpaces TagName [Either Spaces Attribute]
pTagOpen :: PU CTag
pTagOpen = do
  char '<'
  tn   <- pTagName
  sOrA <- many spacesOrAttr <?> "parse attribute error"
  char '>'
  case Map.lookup tn supportedName of
    Nothing  -> return $ CTag NotDecidedTagMark (Right tn) (mergeSpaces sOrA) NotDecidedCloseMark
    Just tn' -> return $ CTag NotDecidedTagMark (Left tn') (mergeSpaces sOrA) NotDecidedCloseMark
  <?> "pTagOpen"

pTagClose :: PU String
pTagClose = do {string "</"; tn <- pTagName; char '>'; return $ tn} <?> "pTagClose"

pTagText :: PU HTML
pTagText = do
  c <- lookAhead (anyChar)
  case c of
    '<' -> unexpected (Label ((DLN.:|) '<' []) )
    _   -> do
          text <- someTill anyChar (lookAhead (string "<") )
          return $ GTreeLeaf (CTagText NotDecidedTextMark (TR text))
  <?> "pTagText"


pTagComment :: PU HTML
pTagComment = do
  string "<!--"
  s <- lookAhead (replicateM 3 anyChar)
  case s of
    "-->" -> string "-->" >> (return $ GTreeLeaf (CTagComment ""))
    _     -> someTill anyChar (string "-->") >>= \cmt -> return $ GTreeLeaf (CTagComment cmt)


-- warning. comments inside the code block is not handled. delay the handle of it to the further transformation after parsing.
-- eg: <code> <!-- sdfdsf --> is not handled </code>
pTagCode :: PU (GTree CTag)
pTagCode = do
  CTag _ (Left tn) sOrA _ <- pTagOpen
  if tn == CCode
    then do
    code <- someTill anyChar (string "</code>")
    return $ GTreeNode (CTag Inline (Left CCode) sOrA NormalClose) [GTreeLeaf (CCodeContent code)]
    else unexpected (Label ((DLN.:|) 'c' []) )


spacesOrAttr :: PU (Either Spaces Attribute)
spacesOrAttr = (spaceChar >>= return . Left . flip (:) []) <|> (pAttribute >>= return . Right)
  <?> "error when parsing attributes"

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
  return $ Attribute an (s0 ++ "=" ++ s1) av
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
  CTag _ tn sOrA maybeSelfClose <- pTagSelfCloseOrOpen
  if (prtCTagName tn) == "script"
  then  case maybeSelfClose of
          SelfClose -> return $ GTreeLeaf (CTagScript $ "<" ++ (prtCTagName tn) ++ flatSorAs sOrA  ++ "/>")
          NotDecidedCloseMark -> do
            txt <- manyTill anyChar (string "</script>")
            return $ GTreeLeaf (CTagScript $ wrapTagO ((prtCTagName tn) ++ flatSorAs sOrA) ++ txt ++ "</script>")
  else failure (Set.singleton (Tokens (DLN.fromList "not a script tag, try other parsers"))) (Set.empty) (Set.empty)



spaceChars :: PU String
spaceChars = many spaceChar <?> "spaceChars"


-- merge adjacent spaces into a sequence of spaces
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


-- void elements
-- these elements has no closing tag. but we permit either <area ... >, <area ... />, or <area ...></area>
isVoidElement :: String -> Bool
isVoidElement = (`elem` ["area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link", "meta", "param", "source", "track", "wbr"])



------------------------
-- printing function from CST to HTML
prtDocument :: HTMLDoc -> String
prtDocument (HTMLDoc pre doctype mid html tra) = pre ++ doctype ++ mid ++ prtHTML html ++ tra

prtDocumentBody :: HTMLDoc -> String
prtDocumentBody (HTMLDoc pre doctype mid (GTreeNode _ ele) tra) = concatMap prtHTML (findBody ele)
    where findBody [] = error "no <body> in html document"
          findBody ((GTreeNode (CTag Block (Right "body") [] NormalClose) bodyContent) : res) = bodyContent
          findBody (x:xs) = findBody xs

prtHTML :: HTML -> String
prtHTML (GTreeNode (CTag _ tn sOrAs SelfClose) [])     = "<" ++ prtCTagName tn ++ flatSorAs sOrAs ++ "/>"
prtHTML (GTreeNode (CTag _ tn sOrAs NoClose) [])     = "<" ++ prtCTagName tn ++ flatSorAs sOrAs ++ ">"
prtHTML (GTreeNode (CTag _ tn sOrAs NormalClose) children) = wrapTagO (prtCTagName tn ++ flatSorAs sOrAs) ++ concatMap prtHTML children ++ wrapTagC (prtCTagName tn)
prtHTML (GTreeLeaf (CTag _ tn sOrAs SelfClose))     = "<" ++ prtCTagName tn ++ flatSorAs sOrAs ++ "/>"
prtHTML (GTreeLeaf (CTag _ tn sOrAs NoClose))     = "<" ++ prtCTagName tn ++ flatSorAs sOrAs ++ ">"
prtHTML (GTreeLeaf (CTagText _ (TL entity))) = prtEntity entity
prtHTML (GTreeLeaf (CTagText _ (TM spaces))) = spaces
prtHTML (GTreeLeaf (CTagText _ (TR str))) = str
prtHTML (GTreeLeaf (CTagComment cmt)) = "<!--" ++ cmt ++ "-->"
prtHTML (GTreeLeaf (CTagScript txt)) = txt
prtHTML (GTreeLeaf (CCodeContent code)) = code

prtEntity :: Entity -> String
prtEntity EntitySpace1 = "&nbsp;"
prtEntity EntitySpace2 = "&#160;"
prtEntity EntityLT1 = "&lt;"
prtEntity EntityLT2 = "&#60;"
prtEntity EntityGT1 = "&gt;"
prtEntity EntityGT2 = "&#62;"
prtEntity EntityAmp1 = "&amp;"
prtEntity EntityAmp2 = "&#38;"

------- entities to be supported ----------
--EntityCent1
--EntityCent2
--EntityPound1
--EntityPound2
--EntityYen1
--EntityYen2
--EntityEuro1
--EntityEuro2
--EntityCopy1
--EntityCopy2
--EntityReg1
--EntityReg2

-- ¢ cent  "&cent";  "&#162";
-- £ pound "&pound"; "&#163";
-- ¥ yen "&yen"; "&#165";
-- € euro  "&euro";  "&#8364";
-- © copyright "&copy";  "&#169";
-- ® registered trademark  "&reg"; "&#174";

-- &#768;
-- &#769;
--̂ &#770;
-- &#771;
------- entities to be supported ----------

flatSorAs :: [Either Spaces Attribute] -> String
flatSorAs [] = ""
flatSorAs (Left sps : xs) = sps ++ flatSorAs xs
flatSorAs (Right (Attribute name eq val) : xs) = name ++ eq ++ val ++ flatSorAs xs

prtCTagName :: Either SupportedName OtherName -> String
prtCTagName (Left sptdname) = prtSupportedName sptdname
prtCTagName (Right othername) = othername

prtSupportedName :: SupportedName -> String
prtSupportedName CDiv = "div"   ; prtSupportedName CPara = "p"
prtSupportedName CCode = "code" ; prtSupportedName CPre  = "pre"
prtSupportedName CBlockQuote = "blockquote"
prtSupportedName COrderedList    = "ol"; prtSupportedName CUnorderedList  = "ul";
prtSupportedName CDefinitionList = "dl"; prtSupportedName CListItem   = "li"
prtSupportedName (CHead i) = "h" ++ show i
prtSupportedName CHorizontalRule = "hr"
prtSupportedName CTable       = "table"; prtSupportedName CTableHeader = "th"
prtSupportedName CTableRow    = "tr"   ; prtSupportedName CTableCell   = "td"
prtSupportedName CBr = "br"
prtSupportedName CEmph = "em"; prtSupportedName CStrong = "strong"
prtSupportedName CStrike      = "strike"
prtSupportedName CSuperscript = "sup"; prtSupportedName CSubscript = "sub"
prtSupportedName CQuoted = "q"       ; prtSupportedName CCite = "cite"
prtSupportedName CLink = "a"         ; prtSupportedName CImg = "img"



------------------------

-- transfer the entities to the corresponding constructors.
recogEntities :: HTML -> Writer [String] HTML
recogEntities t@(GTreeNode (CTag mk0 tn attrs mk1) subtags) =
  if isSubtreeInline t
    then do res <- mapM recogEntities2 subtags
            return (GTreeNode (CTag mk0 tn attrs mk1) (concat res))
    else liftM (GTreeNode (CTag mk0 tn attrs mk1)) (mapM recogEntities subtags)
recogEntities t = return t


-- code not handled yet
recogEntities2 :: HTML -> Writer [String] [HTML]
recogEntities2 t@(GTreeLeaf (CTagText mk0 (TR str))) = recogEntities3 mk0 [] str >>= \res -> return (concat res)
recogEntities2 (GTreeNode (CTag mk0 tn attrs mk1) subtag) = mapM recogEntities2 subtag >>= \res -> return [GTreeNode (CTag mk0 tn attrs mk1) (concat res)]
-- recogEntities (GTreeLeaf (CCodeContent str)) = GTreeLeaf (CCodeContent (recogEntities3 str))
recogEntities2 t = return [t]

recogEntities3 :: TextMark -> String -> String -> Writer [String] [[HTML]]
recogEntities3 mk acc str = case str of
  ('&':'n':'b':'s':'p':';' :rem) -> recogEntities3 mk [] rem >>= \val -> return $ mkData mk acc EntitySpace1 val
  ('&':'#':'1':'6':'0':';' :rem) -> liftM (mkData mk acc EntitySpace2) (recogEntities3 mk [] rem)
  ('&':'l':'t':';' :rem)         -> liftM (mkData mk acc EntityLT1) (recogEntities3 mk [] rem)
  ('&':'#':'6':'0':';' :rem)     -> liftM (mkData mk acc EntityLT2) (recogEntities3 mk [] rem)
  ('&':'g':'t':';' :rem)         -> liftM (mkData mk acc EntityGT1) (recogEntities3 mk [] rem)
  ('&':'#':'6':'2':';' :rem)     -> liftM (mkData mk acc EntityGT2) (recogEntities3 mk [] rem)
  ('&':'a':'m':'p':';' :rem)     -> liftM (mkData mk acc EntityAmp1) (recogEntities3 mk [] rem)
  ('&':'#':'3':'8':';' :rem)     -> liftM (mkData mk acc EntityAmp2) (recogEntities3 mk [] rem)
  ('&':rem)                      -> do
    tell $ ["warning: not supported entities found: " ++ findEntity rem [] ++ " well-behavedness is not guaranteed."]
    recogEntities3 mk ('&':acc) rem
  (x:rem)                        -> recogEntities3 mk (x:acc) rem
  []                             -> return $ (if null acc then [] else [GTreeLeaf (CTagText mk (TR (reverse acc)))]) : []
  where
    mkData mk acc entity val = (if null acc then [] else [GTreeLeaf (CTagText mk (TR (reverse acc)))]) : [GTreeLeaf (CTagText mk (TL entity))] : val
    findEntity (';': xs) acc = ('&' : reverse acc) ++ ";"
    findEntity (x:xs) acc = findEntity xs (x:acc)


refineDoc :: HTMLDoc -> HTMLDoc
refineDoc (HTMLDoc s0 doctype s1 html s2) =
  let (res, warns) = runWriter (recogEntities html)
      trace' s v = if null s then v else trace s v
  in  trace' (unlines warns) $ HTMLDoc s0 doctype s1 (markTextNodeType res) s2

-- to distinguish Inline TextNode and Other TextNode.
-- to merge adjacent spaces together.
markTextNodeType :: HTML -> HTML
markTextNodeType t@(GTreeNode (CTag mk0 tn attrs mk1) subtags) =
  if isSubtreeInline t
    then GTreeNode (CTag mk0 tn attrs mk1) (concatMap markInlineTag subtags)
    else GTreeNode (CTag mk0 tn attrs mk1) (map markTextNodeType subtags)
markTextNodeType (GTreeLeaf (CTagText NotDecidedTextMark (TR str))) = GTreeLeaf (CTagText OtherText (TR str))
markTextNodeType t = t

--TL Entity | TM Spaces | TR String
markInlineTag :: GTree CTag -> [GTree CTag]
markInlineTag (GTreeLeaf (CTagText NotDecidedTextMark (TR str))) = concatMap divideAtLineBreak . map markSpace . sepString $ str
-- eg:  (GTree (CTag Inline CStrong attrs NormalClose) subtext...)
-- CCodeContent is already set to Inline during the parsing stage.
markInlineTag (GTreeNode (CTag Inline tn attrs mk1) subtags) = [(GTreeNode (CTag Inline tn attrs mk1)) (concatMap markInlineTag subtags)]
markInlineTag (GTreeLeaf (CTagText NotDecidedTextMark (TL entity))) = [GTreeLeaf (CTagText InlineText (TL entity))]
markInlineTag c = [c]
--markInlineTag t = error $ "unexpected tag. tag should be text tag or inline tag: " ++ show t


-- separate a string by spaces, and make each substring a GTree (CTagText InlineText ...) []
sepString :: String -> [GTree CTag]
sepString = map (\str -> GTreeLeaf (CTagText InlineText (TR str))) . groupBySpaces
  where groupBySpaces :: String -> [String]
        groupBySpaces = groupBy (\x y -> isSpace x && isSpace y || not (isSpace x) && not (isSpace y))
  -- eg: groupBy predicate "ab \t\t  defsdfsdf          g"   ---->   ["ab"," \t\t  ","defsdfsdf","          ","g"]


-- mark a (GTree (CTagText InlineText ...)...) as either (Left Spaces) if it is space string or Right String.
markSpace :: GTree CTag -> GTree CTag
markSpace (GTreeLeaf (CTagText InlineText (TR ss@(spc:spcs)))) =
  if isSpace spc then GTreeLeaf (CTagText InlineText (TM ss)) else GTreeLeaf (CTagText InlineText (TR ss))

-- given a list of space characters such as " ", "\n", "\t"... break it into several parts at "\n".
-- since "\n" is treated as SoftBreak.
divideAtLineBreak :: GTree CTag -> [GTree CTag]
divideAtLineBreak gt@(GTreeLeaf (CTagText InlineText (TR _))) = [gt]
divideAtLineBreak (GTreeLeaf (CTagText InlineText (TM spaces))) = divideAtLineBreak_ spaces
  where divideAtLineBreak_ spaces =
          let (a, b) = span (/='\n') spaces
          in  case a of
                "" -> case b of
                        '\n':remains -> GTreeLeaf (CTagText InlineText (TM "\n")) : divideAtLineBreak_ remains
                        ""           -> []
                _  -> case b of
                        '\n':remains -> GTreeLeaf (CTagText InlineText (TM a)) :
                                        GTreeLeaf (CTagText InlineText (TM "\n")) : divideAtLineBreak_ remains
                        ""           -> [GTreeLeaf (CTagText InlineText (TM a))]


isSubtreeInline :: GTree CTag -> Bool
isSubtreeInline (GTreeNode (CTag _ (Left tn) _ _) _) = tn `elem` [CPara, CHead 1, CHead 2, CHead 3, CHead 4, CHead 5, CHead 6, CCode]
isSubtreeInline _ = False


------------------------
parseHTML :: String -> HTMLDoc
parseHTML src = refineDoc $ either (error . parseErrorPretty) id (runParser parseDoc "" src)

emptyHTMLCST :: HTMLDoc
emptyHTMLCST = HTMLDoc ""  doctype " " html "\n"
      where doctype = "<!DOCTYPE HTML>"
            html    = (GTreeNode (CTag Block (Right "html") [] NormalClose)
                             [GTreeLeaf (CTagText OtherText (TR "\n"))
                             ,GTreeNode (CTag Block (Right "head") [] NormalClose) [GTreeLeaf (CTagText OtherText (TR "\n"))]
                             ,GTreeLeaf (CTagText OtherText (TR "\n  "))
                             ,GTreeNode (CTag Block (Right "body") [] NormalClose) []])

emptyHTMLStr = "<!DOCTYPE HTML>\n<html>\n<head>\n</head>\n<body>\n</body>\n</html>"

t1 :: IO ()
t1 = do
  i <- readFile "test111.html"
  let oo  = parseHTML i
  putStrLn "html CST:"
  putStrLn (ppShow oo)
  putStrLn "print CST back:"
  putStrLn (prtDocument oo)
  putStrLn "\nequal?\n"
  putStrLn (show (prtDocument oo == i))

t1w :: IO ()
t1w = do
  i <- readFile "1.html"
  let o = either (error. show) show (parse parseDoc "1.html" i)
  writeFile "1o.html" o
