BiPandoc
==========

How to run this prototype?

```haskell
$ ghci -XTemplateHaskell
Prelude> :l TestBX.hs
*TestBX> test "test.md"
```

The interface:

```haskell
-- module Markdown
parseMarkdown :: String -> MarkdownDoc
printMarkdown :: MarkdownDoc -> String

-- module BX
markdownBX :: BiGUL MarkdownDoc AbsDocument
```

Please see the `BiPandoc-Proposal.markdown` for more information.