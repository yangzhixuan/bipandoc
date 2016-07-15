import Text.Pandoc.Options
import Text.Pandoc.Error
import Text.Pandoc.Readers.HTML (readHtml)
import Text.Pandoc.Writers.HTML

fromRight :: Either a b -> b
fromRight (Right x) = x

test1 = do
  stream <- readFile "1.html"
  let ast = fromRight (readHtml (def :: ReaderOptions) stream)
  putStrLn (show ast)
  let outHTML = writeHtmlString (def :: WriterOptions) ast
  putStrLn "\n"
  putStrLn outHTML



--Pandoc (Meta {unMeta = fromList [("title",MetaInlines [Str "Free",Space,Str "Online",Space,Str "Template",Space,Str "Builder"]),("verify-v1",MetaInlines [Str "J39vDqjiZ+Ikdp5/9odQGYjSsaBlh5et5VFfOx5nzpE="])]}) [Para [Image ("",[],[]) [Str "hehe"] ("123123",""),SoftBreak,Link ("",[],[]) [Str "123"] ("%20123.png","")],Table [] [AlignDefault,AlignDefault] [0.0,0.0] [] [[[Plain [Str "1"]],[Plain [Str "3"]]]],BulletList [[Plain [Str "a"]],[Plain [Str "b"]],[Plain [Str "c"]],[Plain [Str "d"]]]]