import qualified Generics.BiGUL
import qualified Generics.BiGUL.Interpreter as BiGUL
import qualified Generics.BiGUL.TH
import Generics.BiGUL.Error (BiGULTrace)


import Abstract

import qualified BX.HTMLBX as HTMLBX
import qualified BX.MarkdownBX as MarkdownBX

import qualified Parser.HTMLParser as HTMLParser
import qualified Parser.Markdown as MarkdownParser

import Data.Maybe
import System.Environment
import System.IO
import System.Exit
import Text.Show.Pretty
import qualified Options.Applicative as OA
import Options.Applicative ((<>), (<$>), (<*>))

import Debug.Trace 

data Options = Options { srcFormat :: String, dstFormat :: String, dstFile :: String, outputFile :: String, checkAmbiguity :: Bool, srcFile :: String } deriving (Show)

withDefaultValue a = fmap (fromMaybe a)

optsParser = Options <$> OA.strOption ( OA.long "from" <> OA.short 'f' <> OA.metavar "FORMAT" <> OA.help "Source format")
                     <*> OA.strOption ( OA.long "to" <> OA.short 't' <> OA.metavar "FORMAT" <> OA.help "Destination format")
                     <*> withDefaultValue "" (OA.optional (OA.strOption ( OA.long "dst" <> OA.short 'd' <> OA.metavar "FILENAME" <> OA.help "Destination filename. Use an empty document if not specified.")))
                     <*> withDefaultValue "" (OA.optional (OA.strOption ( OA.long "output" <> OA.short 'o' <> OA.metavar "FILENAME" <> OA.help "Output filename")))
                     <*> OA.switch (OA.long "check-ambiguity" <> OA.help "Check the format ambiguity when printing")
                     <*> withDefaultValue "" (OA.optional $ OA.argument OA.str (OA.metavar "FILE"))

optsWithInfo = OA.info (OA.helper <*> optsParser) (OA.fullDesc <> OA.progDesc "Supported formats: markdown, html\n" <> OA.header "bipandoc - a bidirectional document converter")


get :: Options -> String -> Maybe AbsDocument
get opt src = case srcFormat opt of
    "html" -> BiGUL.get HTMLBX.htmlBX (HTMLParser.parseHTML src)
    "markdown" -> BiGUL.get MarkdownBX.markdownBX (MarkdownParser.parseMarkdown (addNewline src))
    f -> error ("Invalid source format: " ++ f)
    where addNewline s = if null s || last s /= '\n' then s ++ "\n" else s

getTrace :: Options -> String -> BiGULTrace
getTrace opt src = case srcFormat opt of
    "html" -> BiGUL.getTrace HTMLBX.htmlBX (HTMLParser.parseHTML src)
    "markdown" -> BiGUL.getTrace MarkdownBX.markdownBX (MarkdownParser.parseMarkdown (addNewline src))
    f -> error ("Invalid source format: " ++ f)
    where addNewline s = if null s || last s /= '\n' then s ++ "\n" else s


put :: Options -> String -> AbsDocument -> Maybe String
put opt src view = case dstFormat opt of 

    "html" -> put' (HTMLBX.htmlBX, HTMLParser.parseHTML, HTMLParser.prtDocument)

    "html-body" -> put' (HTMLBX.htmlBX, HTMLParser.parseHTML, HTMLParser.prtDocumentBody)

    "markdown" -> put' (MarkdownBX.markdownBX, MarkdownParser.parseMarkdown, MarkdownParser.printMarkdown)

    f -> error ("Invalid target format: " ++ f)

    where put' (bx, parser, printer) = do
            src' <- BiGUL.put bx (parser src) view
            let result = printer src'
            if not (checkAmbiguity opt)
               then return result
               else if BiGUL.get bx (parser result) /= Just view
                       then do 
                           traceM "Format ambiguity detected! get(parser(result)) /= original_ast"
                           traceM $ "the original view is:\n" ++ (ppShow (Just view))
                           traceM "vs"
                           traceM $ "get(parser(output)) is:\n" ++ (ppShow $ BiGUL.get bx (parser result))
                           return result
                       else return result

putTrace :: Options -> String -> AbsDocument -> BiGULTrace
putTrace opt src view = case dstFormat opt of 

    "html" -> put' (HTMLBX.htmlBX, HTMLParser.parseHTML, HTMLParser.prtDocument)

    "markdown" -> put' (MarkdownBX.markdownBX, MarkdownParser.parseMarkdown, MarkdownParser.printMarkdown)

    f -> error ("Invalid target format: " ++ f)

    where put' (bx, parser, printer) = BiGUL.putTrace bx (parser src) view

defaultDocument :: String -> String
defaultDocument format = 
    case format of
        "html" -> HTMLParser.emptyHTMLStr
        "html-body" -> HTMLParser.emptyHTMLStr
        "markdown" -> MarkdownParser.defaultMarkdown
        _ -> ""

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
           print $ getTrace opts src
           exitFailure
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
                  putStrLn "Failed to put-back into target, see trace:"
                  print $ putTrace opts dst view
                  exitFailure
              else do
                  let (Just target) = targetM
                  outH <- if outputFile opts == "" 
                             then return stdout
                             else openFile (outputFile opts) WriteMode
                  hPutStr outH target
                  hClose outH
                  exitSuccess
