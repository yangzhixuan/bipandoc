BiPandoc
==========

The Command-line Wrapper
---------
Build:

```
ghc --make bipandoc.hs -outputdir build
```

Run:

```
./bipandoc -f html -t markdown 1.html
```

See `./bipandoc --help` for more options

Code for Developers
----------

```haskell
$ ghci -XTemplateHaskell
Prelude> :l TestBX.hs
*TestBX> test "test.md"
```

The interface:

```haskell
-- module Parser.Markdown
parseMarkdown :: String -> MarkdownDoc
printMarkdown :: MarkdownDoc -> String

-- module BX.MarkdownBX
markdownBX :: BiGUL MarkdownDoc AbsDocument
```
