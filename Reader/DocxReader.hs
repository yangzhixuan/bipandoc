{-# LANGUAGE ViewPatterns #-}

module Reader.DocxReader where

import Text.Show.Pretty (ppShow)
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
import qualified Text.XML.HXT.Core as T (mkName)
import Codec.Archive.Zip
import Path.Internal
import Data.ByteString.UTF8
import System.Environment
import qualified Debug.Trace as DT
import Data.Map

import Abstract

-- dataStructure of Docx CST

type DocxCST = [Block]

data ReDocxCST = Docx [Block] deriving (Eq, Show)

data Block = Para [Inline]
           | Table [Tr]
           | OrderedList [ReListItem]
           | UnorderedList [ReListItem]
           | ListItem Int Int [Inline] -- level, numId, content
           deriving (Eq, Show)

data ReListItem = OrderedListItem Int Int [Block] -- level, numId, content
                | UnorderedListItem Int Int [Block]  -- same
                deriving (Eq, Show)

data Tr = Tr [Tc] deriving (Eq, Show)
data Tc = Tc [Block] deriving (Eq, Show)

data Inline = Text String
            | Bold [Inline]
            | Emph [Inline]
            | Strike [Inline]
            | Tab
            | Br
            | Cr
            | Link String [Inline]  --address, text
            | Drawing
            deriving (Eq, Show)


-- deal with the document.xml

  -- deal with Inline element

getInlineContent :: XmlTrees -> [Inline]
getInlineContent ((NTree node sons):ns) =
  case node of XTag ((T.mkName "w:tab" ==) -> True) tagContent -> [Tab] ++ (getInlineContent ns)
               XTag ((T.mkName "w:br" ==) -> True) tagContent -> [Br] ++ (getInlineContent ns)
               XTag ((T.mkName "w:cr" ==) -> True) tagContent -> [Cr] ++ (getInlineContent ns)
               XTag ((T.mkName "w:t" ==) -> True) tagContent -> (getInlineContent sons) ++ (getInlineContent ns)
               XText str -> [Text str]
               XTag qname tagContent -> getInlineContent ns
getInlineContent [] = []


getInlinePr :: (XmlTrees, [Inline]) -> [Inline]
getInlinePr ([], content) = content
getInlinePr (((NTree node sons):ns), content) =
  case node of XTag ((T.mkName "w:b" ==) -> True) tagContent ->
                 if tagContent == []
                 then [Bold (getInlinePr (ns, content))]
                 else getInlinePr (ns, content)
               XTag ((T.mkName "w:i" ==) -> True) tagContent ->
                 if tagContent == []
                 then [Emph (getInlinePr (ns, content))]
                 else getInlinePr (ns, content)
               XTag ((T.mkName "w:strike" ==) -> True) tagContent ->
                 if tagContent == []
                 then [Strike (getInlinePr (ns, content))]
                 else getInlinePr (ns, content)
               XTag qname tagContent -> getInlinePr (ns, content)


getLinkType :: XmlTrees -> String
getLinkType ((NTree node sons):ns) =
  case node of XAttr ((T.mkName "r:id" ==) -> True) ->
                 case sons of ((NTree (XText str) s'):ss) -> str
                              _ -> ""
               _ -> ""

getInline :: XmlTrees -> Map String String -> [Inline]
getInline [] _ = []
--getInline ((NTree node ((NTree node' sons'):ns')):ns) =
getInline ((NTree node sons):ns) relMap =
  case node of
    XTag ((T.mkName "w:r" ==) -> True) tagContent ->
      case sons of((NTree node' sons'):ns') ->
                    case node' of XTag ((T.mkName "w:rPr" ==) -> True) tagContent ->
                                    let content = getInlineContent ns'
                                    in  getInlinePr (sons', content) ++ getInline ns relMap
                                  XTag ((T.mkName "w:t" ==) -> True) tagContent ->
                                      let content = getInlineContent sons
                                      in getInlinePr ([], content) ++ getInline ns relMap
                                  XTag qname tagContent -> getInline ns relMap
                  [] -> getInline ns relMap
    XTag ((T.mkName "w:hyperlink" ==) -> True) tagContent ->
      let linkType = getLinkType tagContent in
      if linkType == ""
      then getInline ns relMap
      else [Link (relMap ! linkType) (getInline sons relMap)] ++ getInline ns relMap
    XTag qname tagContent -> getInline ns relMap


getListValue :: XmlTree -> Int
getListValue (NTree node sons) =
  case node of XTag qname tagContent -> getListValue (tagContent !! 0)
               XAttr qname -> getListValue (sons !! 0)
               XText str -> (read str) :: Int


getParaPr :: (XmlTrees, [Inline]) -> Block
getParaPr ([], content) = Para content

getParaPr (((NTree node sons):ns), content) =
  case node of
    XTag ((T.mkName "w:numPr" ==) -> True) tagContent ->
      let num_temp = getListValue (sons !! 1) in
      if num_temp == 0
        then getParaPr (ns, content)
      else ListItem (getListValue (sons !! 0)) (getListValue (sons !! 1)) content

    XTag qname tagContent -> getParaPr (ns, content)

getParaBlock :: XmlTrees -> Map String String -> Block
getParaBlock ((NTree node sons):ns) relMap =
  let paraContent = getInline ns relMap in
    --DT.trace("inline " ++ show paraContent) (getParaPr (sons, paraContent))
    getParaPr (sons, paraContent)

getTblBlock :: XmlTrees -> Map String String -> Block
getTblBlock _ _ = Table []

{-getTblBlock [] = []
getTblBlock ((NTree node sons):ns) =
  case node of XTag ((T.mkName "w:tr" ==) -> True) tagContent ->
               XTag qname tagContent -> getTblBlock ns-}

resolve :: (XmlTrees, DocxCST) -> Map String String -> DocxCST
resolve (((NTree node sons):ns), blockList) relMap =
    case node of XTag qname tagContent ->
                    let tagStr = qualifiedName qname in
                    if tagStr == "w:document" || tagStr == "w:body"
                      then resolve (sons, blockList) relMap
                      else
                        if tagStr == "w:p"
                          then resolve (ns, blockList ++ [getParaBlock sons relMap]) relMap
                          else
                            if tagStr == "w:tbl"
                              then resolve (ns, blockList ++ [getTblBlock sons relMap]) relMap
                              else resolve (ns, blockList) relMap

resolve ([], blockList) relMap = blockList

getCST :: [XmlTree] -> Map String String -> DocxCST
getCST ((NTree _ ts) :xs) relMap = resolve (ts, []) relMap


-- deal with the rels of document and get the Map of rels

getRid :: XmlTrees -> String
getRid ((NTree node sons):ns) =
  case node of XAttr ((T.mkName "Id" ==) -> True) ->
                  case sons of ((NTree (XText str) sons):ns') -> str
               _ -> getRid ns

getTarget :: XmlTrees -> String
getTarget ((NTree node sons):ns) =
  case node of XAttr ((T.mkName "Target" ==) -> True) ->
                  case sons of ((NTree (XText str) sons'):ns') -> str
               _ -> getTarget ns

getRels :: [XmlTree] -> Map String String
getRels ((NTree node ((NTree node' sons'):ns')):ns) = fromList $ getRidMap sons'

getRidMap :: XmlTrees -> [(String, String)]
getRidMap [] = []
getRidMap ((NTree node sons):ns) =
  case node of XTag ((T.mkName "Relationship" ==) -> True) tagContent -> [(getRid tagContent, getTarget tagContent)] ++ getRidMap ns
               _ -> getRidMap ns


-- deal with the list information of document.xml in numbering.xml

getListStyle :: [XmlTree] -> (Map String String, Map String String)
getListStyle ((NTree node ((NTree node' sons'):ns')):ns) =
  let temp = dealNumbering sons' [] [] in
  (fromList $ fst temp, fromList $ snd temp)

dealNumbering :: XmlTrees -> [(String, String)] -> [(String, String)] -> ([(String, String)], [(String, String)])
dealNumbering [] l1 l2 = (l1, l2)
dealNumbering ((NTree node sons):ns) l1 l2 =
  case node of
    XTag ((T.mkName "w:abstractNum" ==) -> True) tagContent ->
      let absNumId = getNumId tagContent "w:abstractNumId"
          lvlList = getLvlList sons absNumId
      in dealNumbering ns (l1 ++ lvlList) l2

    XTag ((T.mkName "w:num" ==) -> True) tagContent ->
      let numId = getNumId tagContent "w:numId" in
      case sons of
        ((NTree (XTag ((T.mkName "w:abstractNumId" ==) -> True) tagContent') sons'):ns') ->
          let absNumId = getNumId tagContent' "w:val" in
            dealNumbering ns l1 (l2 ++ [(numId, absNumId)])

    _ -> dealNumbering ns l1 l2


getNumId :: XmlTrees -> String -> String
getNumId ((NTree node sons):ns) attrName =
  case node of XAttr ((T.mkName attrName ==) -> True) ->
                  case sons of ((NTree (XText str) sons'):ns') -> str
               _ -> getNumId ns attrName


getLvlList :: XmlTrees -> String -> [(String, String)]
getLvlList [] _ = []
getLvlList ((NTree node sons):ns) absNumId =
  case node of
    XTag ((T.mkName "w:lvl" ==) -> True) tagContent ->
      let level = getNumId tagContent "w:ilvl"
          style = getStyle sons
      in [((absNumId ++ ":" ++ level), style)] ++ getLvlList ns absNumId

    _ -> getLvlList ns absNumId


getStyle :: XmlTrees -> String
getStyle ((NTree node sons):ns) =
  case node of
    XTag ((T.mkName "w:numFmt" ==) -> True) tagContent -> getNumId tagContent "w:val"
    XTag ((T.mkName "mc:AlternateContent" ==) -> True) tagContent -> getStyle sons
    XTag ((T.mkName "mc:Fallback" ==) -> True) tagContent -> getStyle sons
    _ -> getStyle ns


--rifine the CST

getListItem :: Block -> (Map String String, Map String String) -> ReListItem
getListItem (ListItem level numId content) mapTuple =
  let style = ((fst mapTuple) ! (((snd mapTuple) ! (show numId)) ++ ":" ++ (show level))) in
    if style == "bullet"
      then UnorderedListItem level numId [Para content]
    else OrderedListItem level numId [Para content]

getInitList :: Block -> (Map String String, Map String String) -> [Block]
getInitList item@(ListItem level numId content) mapTuple =
  let style = ((fst mapTuple) ! (((snd mapTuple) ! (show numId)) ++ ":" ++ (show level))) in
    if style == "bullet"
      then [UnorderedList []]
    else [OrderedList []]

addItem :: [Block] -> Block -> (Map String String, Map String String) -> [Block]
addItem list item mapTuple =
  let listEnd = last list
      listInit = init list
  in case listEnd of
       UnorderedList ls -> listInit  ++ [UnorderedList (ls ++ [getListItem item mapTuple])]
       OrderedList ls -> listInit ++ [OrderedList (ls ++ [getListItem item mapTuple])]

addItemByBlock :: [Block] -> Block -> [Block] -> [Block]
addItemByBlock list (ListItem level numId content) blocks =
  let listEnd = last list
      listInit = init list
  in case listEnd of
       UnorderedList ls -> listInit ++ [UnorderedList (ls ++ [UnorderedListItem level numId blocks])]
       OrderedList ls -> listInit ++ [OrderedList (ls ++ [OrderedListItem level numId blocks])]


refineList :: (Map String String, Map String String) -> DocxCST -> [Block] -> (DocxCST, [Block])
-- mapTuple -> rest line -> now list -> (ans list, rest line)

refineList _ [] now_list = (now_list, [])

refineList mapTuple (block:[]) now_list =
  case block of
    ListItem level numId content -> (addItem now_list block mapTuple, [])
    _ -> (now_list, [block])

refineList mapTuple cst@(block:next@(bn:bs)) now_list =
  case block of
    ListItem level numId content ->
      case bn of
        ListItem bLevel bNumId bContent ->
          if bLevel == level && bNumId == numId
            then refineList mapTuple next (addItem now_list block mapTuple)

          else if bLevel == level && not (bNumId == numId)
            then let rest_ans = refineList mapTuple next (getInitList bn mapTuple) in
              (((addItem now_list block mapTuple) ++ fst rest_ans), snd rest_ans)

          else if level < bLevel
            then let rest_ans = refineList mapTuple next (getInitList bn mapTuple)
                     restLines = snd rest_ans
            in if restLines == []
                 --then refineList mapTuple restLines (addItemByBlock (addItem now_list block mapTuple) block (fst rest_ans))
                 then refineList mapTuple restLines (addItemByBlock now_list block ([block]++(fst rest_ans)))
               else
                 let restFirst = head restLines in
                 case restFirst of
                   ListItem rLevel rNumId rContent ->
                     if level > rLevel
                       --then (addItemByBlock (addItem now_list block mapTuple) block (fst rest_ans), restLines)
                       then (addItemByBlock now_list block ([block]++(fst rest_ans)), restLines)
                     else if rLevel == level && rNumId == numId
                       --then refineList mapTuple restLines (addItemByBlock (addItem now_list block mapTuple) block (fst rest_ans))
                       then refineList mapTuple restLines (addItemByBlock now_list block ([block]++(fst rest_ans)))
                     else if rLevel == level && not (rNumId == numId)
                       then let rest_ans' = refineList mapTuple restLines (getInitList restFirst mapTuple) in
                         --(((addItemByBlock (addItem now_list block mapTuple) block (fst rest_ans)) ++ fst rest_ans'), snd rest_ans')
                         (((addItemByBlock now_list block ([block]++(fst rest_ans))) ++ fst rest_ans'), snd rest_ans')
                     else error "The list structure is wrong"

                   --_ -> refineList mapTuple restLines (addItemByBlock (addItem now_list block mapTuple) block (fst rest_ans))
                   _ -> refineList mapTuple restLines (addItemByBlock now_list block ([block]++(fst rest_ans)))


          else if level > bLevel
            then (addItem now_list block mapTuple, next)
          else error "The list structure is wrong"

        _ -> (addItem now_list block mapTuple, next)

    _ -> ([], cst)


refine :: DocxCST -> (Map String String, Map String String) -> DocxCST
refine [] _ = []
refine cst@(block:bs) mapTuple =
  case block of
    ListItem level numId content ->
      let listAns = refineList mapTuple cst (getInitList block mapTuple)
      in fst listAns ++ (refine (snd listAns) mapTuple)

    _ -> [block] ++ refine bs mapTuple


--refine str to make it like Markdown

findNextNonStr :: [AbsInline] -> String -> (String, [AbsInline])
findNextNonStr [] str = (str, [])
findNextNonStr (inline:is) str =
  case inline of
    AbsStr text -> findNextNonStr is (str ++ text)
    _ -> (str, (inline:is))


getRefinedStr :: String -> String -> String -> [AbsInline]
getRefinedStr [] now_str lastChar = [AbsStr now_str]
getRefinedStr (c:cs) now_str lastChar =
  if not (c == ' ') then
    if not (lastChar == " ") then getRefinedStr cs (now_str ++ [c]) [c]
    else [AbsStr " "] ++ getRefinedStr cs [c] [c]
  else
    if not (lastChar == " ") then
      if not (now_str == "") then [AbsStr now_str] ++ getRefinedStr cs [c] [c]
      else getRefinedStr cs [c] [c]
    else getRefinedStr cs [c] [c]


refineStr :: [AbsInline] -> [AbsInline]
refineStr [] = []
refineStr (inline:is) =
  case inline of
    AbsLink texts target -> [AbsLink (refineStr texts) target] ++ (refineStr is)
    AbsEmph inlines -> [AbsEmph (refineStr inlines)] ++ (refineStr is)
    AbsStrong inlines -> [AbsStrong (refineStr inlines)] ++ (refineStr is)
    AbsStr str ->
      let find_ans = findNextNonStr (inline:is) []
          raw_str = fst find_ans
          nextInlines = snd find_ans
      in (getRefinedStr raw_str "" "") ++ (refineStr nextInlines)
    _ -> [inline] ++ (refineStr is)


-- transfer CST to AST

transferInline :: [Inline] -> [AbsInline]
transferInline [] = []
transferInline (inline:is) =
  case inline of
    Text str -> [AbsStr str] ++ (transferInline is)
    Bold subInlines -> [AbsStrong (transferInline subInlines)] ++ (transferInline is)
    Emph subInlines -> [AbsEmph (transferInline subInlines)] ++ (transferInline is)
    Strike subInlines -> (transferInline subInlines) ++ (transferInline is)
    Tab -> [AbsStr " "] ++ (transferInline is)
    Br -> [AbsHardbreak] ++ (transferInline is)
    Cr -> [AbsHardbreak] ++ (transferInline is)
    Link address texts -> [AbsLink (transferInline texts) address] ++ (transferInline is)


transferListItem :: [ReListItem] -> [AbsListItem]
transferListItem [] = []
transferListItem (item:is) =
  case item of
    OrderedListItem _ _ blocks -> [AbsOrderedListItem (transferToAST blocks)] ++ (transferListItem is)
    UnorderedListItem _ _ blocks -> [AbsUnorderedListItem (transferToAST blocks)] ++ (transferListItem is)


transferBlock :: Block -> [AbsBlock]
transferBlock block =
  case block of
    ListItem _ _ inlines -> [AbsPara (refineStr $ transferInline inlines)]
    Para inlines -> [AbsPara (refineStr $ transferInline inlines)]
    Table _ -> []
    OrderedList items -> [AbsOrderedList (transferListItem items)]
    UnorderedList items -> [AbsUnorderedList (transferListItem items)]


transferToAST :: [Block] -> [AbsBlock]
transferToAST [] = []
transferToAST (block:bs) = (transferBlock block) ++ (transferToAST bs)


-- get ByteString of XML file from .docx by docxFileName and XMLfilePath in docx

dealInput :: String -> String -> IO ByteString
dealInput filePath xmlFile =
  let Just entrySelector = mkEntrySelector (Path xmlFile) :: Maybe EntrySelector
  in withArchive (Path filePath) (getEntry entrySelector)


getEntrySelector :: String -> String -> EntrySelector
getEntrySelector filePath xmlFile =
  let Just entrySelector = mkEntrySelector (Path xmlFile) :: Maybe EntrySelector
  in entrySelector


getDocxCST :: String -> IO DocxCST
getDocxCST fileName = do
  --argList <- getArgs
  documentByteStr <- dealInput fileName "word/document.xml"
  relByteStr <- dealInput fileName "word/_rels/document.xml.rels"
  --numberingByteStr <- dealInput fileName "word/numbering.xml"
  relTrees <- runX $ readString [withValidate no] (toString relByteStr)
  --numberingTrees <- runX $ readString [withValidate no] (toString numberingByteStr)
  xmlTrees <- runX $ readString [withValidate no] (toString documentByteStr)

  flag <- withArchive (Path fileName) (doesEntryExist $ getEntrySelector fileName "word/numbering.xml")
  if flag then do
    numberingByteStr <- dealInput fileName "word/numbering.xml"
    numberingTrees <- runX $ readString [withValidate no] (toString numberingByteStr)
    return $ refine (getCST xmlTrees (getRels relTrees)) (getListStyle numberingTrees)
  else
    return $ getCST xmlTrees (getRels relTrees)

  --return $ refine (getCST xmlTrees (getRels relTrees)) (getListStyle numberingTrees)
  --putStrLn . ppShow $ getListStyle numberingTrees
  --print numberingTrees
  --print (getCST xmlTrees (getRels relTrees))
  --putStrLn $ ppShow (AbsDocument (transferToAST $ refine (getCST xmlTrees (getRels relTrees)) (getListStyle numberingTrees)))
  --putStrLn . ppShow $ transferToAST $ refine (getCST xmlTrees (getRels relTrees)) (getListStyle numberingTrees)


getDocxAST :: DocxCST -> AbsDocument
getDocxAST = AbsDocument . transferToAST
