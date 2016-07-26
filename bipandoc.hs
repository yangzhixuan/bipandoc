import qualified Generics.BiGUL
import qualified Generics.BiGUL.Interpreter as BiGUL
import qualified Generics.BiGUL.TH

import Abstract

import qualified BX.HTMLBX as HTMLBX
import qualified BX.MarkdownBX as MarkdownBX

import qualified Parser.HTMLParser as HTMLParser
import qualified Parser.Markdown as MarkdownParser

import Data.Maybe
import System.Environment
import System.IO
import qualified Options.Applicative as OA
import Options.Applicative ((<>), (<$>), (<*>))

import Debug.Trace 

data Options = Options { srcFormat :: String, dstFormat :: String, dstFile :: String, outputFile :: String, srcFile :: String } deriving (Show)

optsParser = Options <$> OA.strOption ( OA.long "from" <> OA.short 'f' <> OA.metavar "FORMAT" <> OA.help "Source format")
                    <*> OA.strOption ( OA.long "to" <> OA.short 't' <> OA.metavar "FORMAT" <> OA.help "Destination format")
                    <*> (fmap (fromMaybe "") $ OA.optional (OA.strOption ( OA.long "dst" <> OA.short 'd' <> OA.metavar "FILENAME" <> OA.help "Destination filename. Use an empty document if not specified.")))
                    <*> (fmap (fromMaybe "") $  OA.optional (OA.strOption ( OA.long "output" <> OA.short 'o' <> OA.metavar "FILENAME" <> OA.help "Output filename")))
                    <*> (fmap (fromMaybe "") $ OA.optional $ OA.argument OA.str (OA.metavar "FILE"))

optsWithInfo = OA.info (OA.helper <*> optsParser) (OA.fullDesc <> OA.progDesc "Supported formats: markdown, html" <> OA.header "bipandoc - a bidirectional document converter")



get :: Options -> String -> Maybe AbsDocument
get opt src = case srcFormat opt of

    "html" -> BiGUL.get HTMLBX.htmlBX (HTMLParser.parseHTML src)

    "markdown" -> BiGUL.get MarkdownBX.markdownBX (MarkdownParser.parseMarkdown src)

    f -> error ("Invalid source format: " ++ f)


put :: Options -> String -> AbsDocument -> Maybe String
put opt src view = case dstFormat opt of 

    "html" -> put' (HTMLBX.htmlBX, HTMLParser.parseHTML, HTMLParser.prtDocument)

    "markdown" -> put' (MarkdownBX.markdownBX, MarkdownParser.parseMarkdown, MarkdownParser.printMarkdown)

    f -> error ("Invalid target format: " ++ f)

    where put' (bx, parser, printer) = do
            src' <- BiGUL.put bx (parser src) view
            return $ printer src'

defaultDocument :: String -> String
defaultDocument format = 
    case format of
        "html" -> HTMLParser.defaultHTML
        "markdown" -> MarkdownParser.defaultMarkdown
        _ -> ""

testOpts = Options "html" "markdown" "1.html" "" "abc.md"

main = do
    opts <- OA.execParser optsWithInfo

    -- Read source
    srcH <- if srcFile opts == "" 
               then return stdin
               else openFile (srcFile opts) ReadMode

    src <- hGetContents srcH
    let viewM = get opts src

    -- MarkdownParser.putPretty viewM

    if isNothing viewM
       then do
           putStrLn $ "Failed to get view from " ++ src
           return ()
       else do
           let (Just view) = viewM

           -- Read sychronization target, use a default document if not specied
           dst <- if dstFile opts == ""
                     then return $ defaultDocument (dstFormat opts)
                     else do
                         dstH <- openFile (dstFile opts) ReadMode
                         hGetContents dstH

           let targetM = put opts dst view
           if isNothing targetM 
              then do
                  putStrLn $ "Failed to put-back into target"
                  return ()
              else do
                  let (Just target) = targetM
                  outH <- if outputFile opts == "" 
                             then return stdout
                             else openFile (outputFile opts) WriteMode
                  hPutStr outH target
                  hClose outH
