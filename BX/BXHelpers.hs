
{-# Language TemplateHaskell, TypeFamilies #-}
module BX.BXHelpers where

import Generics.BiGUL
import Data.List
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Control.Arrow ((***))
import Data.Maybe (isJust, catMaybes)



-- |
-- > (==>) = ($)
-- make it more elegant to write ($). Later we may use (==>) instead of ($).
(==>) :: (a -> b) -> a -> b
(==>) = ($)

-- |
-- > emb g p = Case
-- >   [ $(normal [| \s v -> g s == v |] [p| _ |])
-- >     ==> Skip g
-- >   , $(adaptive [| \s v -> True |])
-- >     ==> p
-- >   ]
-- emb g p: invoke g to do the get, and invoke p to do the put.
emb :: Eq v => (s -> v) -> (s -> v -> s) -> BiGUL s v
emb g p = Case
  [ $(normal [| \s v -> g s == v |] [p| _ |])
    ==> Skip g
  , $(adaptive [| \s v -> True |])
    ==> p
  ]

-- |
-- > naiveMap b =
-- >   Case  [ $(normalSV [p| _:_ |] [p| _:_ |]
-- >                      [p| _:_ |])
-- >           ==> $(update [p| x:xs |] [p| x:xs |] [d| x = b; xs = naiveMap b |])
-- >         , $(adaptiveSV [p| _:_ |] [p| [] |] ) (\_ _ -> [])
-- >         , $(normalSV [p| [] |] [p| _:_ |]
-- >                      [| const False |])
-- >           ==> (Fail "length of the view should be less than that of the source.")
-- >         , $(normalSV [p| [] |] [p| [] |]
-- >                      [p| [] |])
-- >           ==> $(update [p| [] |] [p| [] |] [d| |])
-- >         ]
-- A naive map function, which takes a BiGUL program and yields another BiGUL program working on list.
-- The first branch deals with recursive condition.
-- The second branch handles the boundary conditions where the source list is longer than the view list:
-- drop all the remaining elements in the source list and thus make it an empty list.
-- The third branch will throw an error when the view list is longer than the source list.
-- The last branch is the termination condition: both the source and view reach the empty constructor.
--
-- (For the sake of completeness.) In fact 'normalSV' means that we use separate condition for source and view.
-- So we can still use a general function in the predicate:
--
-- > $(normalSV [| \s -> case s of _:_ -> True; _ -> False |] [p| _:_  |] [p| _:_ |])
--
-- >>> put (naiveMap lensSucc) [1,2,3,4] [7,8,9]
-- Right [6,7,8]
--
-- >>> get (naiveMap (lensLength undefined)) ["123", "xyz"]
-- Right [3,3]
--
-- >>>get (naiveMap replaceMin) [(3,9), (-2,10),(10,2)]
-- Right [3,-2,2]

mapLens :: (Show a, Show b) => BiGUL a b -> (b -> a) -> BiGUL [a] [b]
mapLens b create =
  Case  [ $(normalSV [p| _:_ |] [p| _:_ |]
                     [p| _:_ |])
          ==> $(update [p| x:xs |] [p| x:xs |] [d| x = b; xs = mapLens b create |])
        , $(adaptiveSV [p| _:_ |] [p| [] |] ) (\_ _ -> [])
        , $(adaptiveSV [p| [] |] [p| _:_ |])
          ==> \s (v:vs) -> [ create v ]
        , $(normalSV [p| [] |] [p| [] |]
                     [p| [] |])
          ==> $(update [p| [] |] [p| [] |] [d| |])
        ]

-- |
-- > compose = Compose
-- The last combinator we are going to introduce is 'Compose',
-- which takes two BiGUL programs and behaves like \"function composition\".
--
-- Given two BiGUL programs,
--
-- > f :: BiGUL a b, g :: BiGUL b c
-- we have
--
-- > f `Compose` g :: BiGUL a c
-- In the get direction, the semantics of @get (f \`Compose\` g) s@ is:
-- (suppose the function 'get' and 'put' always return a value rather than a value wrapped in 'Right'.)
--
-- > get g (get f s)
-- In the put direction, the semantics of @put (f \`Compose\` g) s v@ is a little bit complex:
--
-- > put f s (put g (get f s) v)
-- Let us make it more clear:
--
-- > let a = get f s
-- >     b = put g a v
-- > in  put f s b
-- Check the type of these transformations by yourself will help you understand deeper.
--
-- Let us try some examples:
--
-- >>> put ((naiveMap replaceMin) `compose` (naiveMap lensSucc)) [(1,-1),(-2,2)] [-8, 1]
-- Right [(1,-9),(0,2)]
--
-- >>> get ((naiveMap replaceMin) `compose` (naiveMap lensSucc)) [(1,-1),(-2,2)]
-- Right [0,-1]
compose :: (Show a, Show b, Show c) => BiGUL a b -> BiGUL b c -> BiGUL a c
compose = Compose

-- |
-- The last example in this tutorial is a simple map-map fusion.
-- It makes the composition of two map functions run more efficiently, compared to using 'Compose' combinator.
--
-- In the get direction, (get (f \`Compose\` g)) traverse the list twice, while (get (mapFusion f g)) traverse the list only once.
-- And in the put direction, (put f \`Compose\` g) traverse the two lists up to five times (get counts up once, two put count up four times, since a put takes two lists as argument),
-- while (put mapFusion f g) traverses the lists only twice.
--
-- Compare the following result (in GHCI)
--
-- > t1 :: Int
-- > t1 = last $ fromRight $ put (naiveMap lensSucc `Compose` naiveMap lensSucc) [1..100000] [2..20001]
-- > t2 :: Int
-- > t2 = last $ fromRight $ put (mapFusion lensSucc lensSucc) [1..100000] [2..20001]
-- > fromRight (Right x) = x
--
-- >>> t1
-- 19999
-- (1.24 secs, 512,471,456 bytes)
--
-- >>> t2
-- 19999
-- (0.23 secs, 122,920,792 bytes)
--
-- More examples can be found in the list library of BiGUL.
mapFusion :: (Show a, Show b, Show c) => BiGUL a b -> BiGUL b c -> BiGUL [a] [c]
mapFusion f g =
  Case  [ $(normalSV [p| _:_ |] [p| _:_ |]
                     [p| _:_ |])
          ==> $(update [p| x:xs |] [p| x:xs |] [d| x = f `Compose` g; xs = mapFusion f g |])
        , $(adaptiveSV [p| _:_ |] [p| [] |] ) (\_ _ -> [])
        , $(normalSV [p| [] |] [p| _:_ |]
                     [| const False |])
          ==> (Fail "length of the view should be less than that of the source")
        , $(normalSV [p| [] |] [p| [] |]
                     [p| [] |])
          ==> $(update [p| [] |] [p| [] |] [d| |])
        ]

filterLens :: (Show a) => (a -> Bool) -> BiGUL [a] [a]
filterLens f = 
    Case [ -- Case 1: [] []
           $(normal [| \s [] -> (null . filter f) s |] [| \s -> (null . filter f) s |] )
           ($(rearrV [| \[] -> () |]) (Skip (const ()))),

           -- Case 1': s:ss [], => make s empty
           $(adaptiveSV [p| _ |] [p| [] |])
           (\s v -> filter (not . f) s),

           -- Case 2: [] v:vs, not f v => fail
           $(normal [| \[] (v:vs) -> not (f v) |] [| const False |])
           (Fail "invalid view"),

           -- Case 3: [] v:vs, f v => add v to source
           $(adaptive [| \[] (v:vs) -> f v |])
           (\s (v:vs) -> [v]),

           -- Case 4: s:ss _, not f s => rearrS
           $(normal [| \(s:ss) _ -> not (f s) |] [| \(s:ss) -> not (f s) |] )
           ($(rearrS [| \(s:ss) -> ss |]) (filterLens f)),

           -- Case 5: s:ss v:vs, f s && f v => Replace and recursion
           $(normal [| \(s:ss) (v:vs) -> (f s) && (f v) |] [| \(s:ss) -> (f s) |] )
           $(update [p| x:xs |] [p| x:xs |] [d| x = Replace; xs = filterLens f |]),

           -- Case 5': s:ss v:vs, f s && not (f v) => fail
           $(normal [| \(s:ss) (v:vs) -> (f s) && not (f v) |] [| const False |] )
           (Fail "invalid view")
        ]


expandLens :: (Eq a, Show a, Eq b, Show b) => Bool -> BiGUL [(a,b)] [(a,[b])]
expandLens check = 
    Case [ -- Case 1: [] []
           $(normalSV [p| [] |] [p| [] |] [p| [] |])
           ==> Skip (const []),

           -- Case 2: [] (view_k, view_v:view_vs):view_kvs => adaptive add (view_k,view_v) to src
           $(adaptive [| \[] ((view_k, view_v:view_vs):view_kvs) -> True |])
           ==> (\src ((view_k, view_v:view_vs):view_kvs) -> (view_k, view_v) : src) ,

           -- Case 3: [] (view_k, []):view_kvs => Fail
           $(normal [| \[] ((view_k, []):view_kvs) -> True |] [| const False |])
           ==> Fail "invalid view 2" ,

           -- Case 4: (src_k, src_v):src_kvs [] => adaptive drop all
           $(adaptive [| \((src_k,src_v):src_kvs) [] -> True |])
           ==> (\src view -> []),

           -- Case 5: (src_k, src_v):src_kvs (view_k, []):view_kvs => Fail
           $(normal [| \((src_k,src_v):src_kvs) ((view_k, []):view_kvs) -> True |] [| const False |])
           ==> Fail "invalid view 1" ,

           -- Case 6 & 7: (src_k, src_v):src_kvs (view_k, view_v:[]):view_kvs 
           --           => If src_v != view_v, then add view_v to src else replace and recursion
           $(adaptive [| \((src_k,src_v):src_kvs) ((view_k, view_v:[]):view_kvs) -> src_k /= view_k || src_v /= view_v |])
           ==> (\src ((view_k, view_v:[]):view_kvs) -> (view_k, view_v) : src) ,

           $(normal [| \((src_k,src_v):src_kvs) ((view_k, view_v:[]):view_kvs) -> src_k == view_k || src_v == view_v |]
                    [| \src -> not check || (length src == 1 || (fst (head src) /= fst (head (tail src))))|])
           ==> $(update [p| (kv:kvs) |] [p| (kv:kvs) |] [d| kv = (Skip (\(k,v) -> (k, [v]))) ; kvs = (expandLens True) |]) ,

           -- Case 8: (src_k, src_v):src_kvs (view_k, view_v:view_v2:view_vs):view_kvs => rearrV
           $(normal [| \((_,_):_) ((_,(_:_:_)):_) -> True |] [| \((k1,_):(k2,_):kvs) -> k1 == k2 |])
           ==> $(rearrV [| \((view_k, (view_v:view_v2:view_vs)):view_kvs) -> (view_k, [view_v]) : (view_k, view_v2:view_vs) : view_kvs |] ) (expandLens False)
          ]

align :: (Show a, Show b)
      => (a -> Bool)       -- ^ source condition
      -> (a -> b -> Bool)  -- ^ matching condition
      -> BiGUL a b         -- ^ inner program
      -> (b -> a)          -- ^ creation
      -> (a -> Maybe a)    -- ^ concealment
      -> BiGUL [a] [b]
align p match b create conceal = Case
  [ $(normal [| \s [] -> (null . filter p) s |] [| \s -> (null . filter p) s |])
    ==> $(rearrV [| \[] -> () |])
         (Skip (const ()))
  , $(adaptiveSV [p| _ |] [p| [] |])
    ==> \ss _ -> catMaybes (map (\s -> if p s then conceal s else Just s) ss)
  -- view is necessarily nonempty in the cases below
  , $(normal [| \(s:_) _ -> not (p s) |] [| \(s:ss) -> (not (p s)) && not ((null . filter p) ss)|])
    ==> $(rearrS [| \(s:ss) -> ss |])
         (align p match b create conceal)
  , $(normal [| \(s:ss) (v:vs) -> p s && match s v |] [| \(s:_) -> p s |])
    ==> $(update [p| x:xs |] [p| x:xs |] [d| x = b; xs = align p match b create conceal |])
  , $(adaptive [| \ss (v:_) -> isJust (findFirst (\s -> p s && match s v) ss) ||
                               let s = create v in p s && match s v |])
    ==> \ss (v:_) -> maybe (create v:ss) (uncurry (:)) (findFirst (\s -> p s && match s v) ss)
  ]
  where
    findFirst :: (a -> Bool) -> [a] -> Maybe (a, [a])
    findFirst p [] = Nothing
    findFirst p (x:xs) | p x       = Just (x, xs)
    findFirst p (x:xs) | otherwise = fmap (id *** (x:)) (findFirst p xs)


groupLens :: BiGUL [(Int,Int)] [(Int ,[Int])]
groupLens = (align (const True) (\x y -> x == y) (Replace) id (const Nothing)) `Compose` (expandLens True)

sortLens :: BiGUL [(Int,Int)] [(Int ,Int)]
sortLens = Skip (\x -> sortBy (\x y -> compare (fst x) (fst y)) x)

filterLens' p = emb (filter p) (filterPut p)
filterPut p s v = 
    if null (filter (not . p) v) 
       then filterPut' s v
       else (error "filterLens: invalid view")
    where filterPut' s [] = filter (not . p) s
          filterPut' [] v = v
          filterPut' (s:ss) (v:vs) = if not (p s) then s : (filterPut' ss (v:vs)) else (v : filterPut' ss vs)

