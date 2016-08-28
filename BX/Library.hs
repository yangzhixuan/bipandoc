{-# Language TemplateHaskell, TypeFamilies #-}

module BX.Library where

import Generics.BiGUL
import Data.List
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Control.Arrow ((***))
import Data.Maybe
import Data.Array.ST
import Data.Array
import Control.Monad.ST
import Control.Monad
import Debug.Trace
import Text.Show.Pretty


-- | make it more elegant to write ($) in 'Case' branches.
(==>) :: (a -> b) -> a -> b
(==>) = ($)

-- | emb g p: invoke g to do the get, and invoke p to do the put.
emb :: Eq v => (s -> v) -> (s -> v -> s) -> BiGUL s v
emb g p = Case
  [ $(normal [| \s v -> g s == v |] [p| _ |])
    ==> Skip g
  , $(adaptive [| \s v -> True |])
    ==> p
  ]

-- | A lens which takes a BiGUL program and yields another BiGUL program working on list.
mapLens :: (Show a, Show b)
        => BiGUL a b            -- ^ The inner BX
        -> (b -> a)             -- ^ The function to create a source element from the view when the source is empty
        -> BiGUL [a] [b]
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

-- | A lens which takes a predicate @ p @ and yields a BiGUL program whose get-direction is equivalent to @ filter p @
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

-- | A lens which tries to align/match the sources with the views, synchronise the matched pairs of sources and views by an inner lens, and process the unmatched sources and views in some programmer-specified ways.
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

-- | A lens whose get-direction is 'sort'. This implementation has an O(n^2) time complexity.
sortLens :: (Show a, Ord a) => BiGUL [a] [a]
sortLens = Case 
    [ $(normalSV [p| [] |] [p| [] |] [p| [] |]) 
      ==> Skip (const []),

      $(adaptiveSV [p| _ |] [p| [] |])
      ==> \s v -> [],

      $(normalSV [p| s:ss |] [p| v:vs |] [p| s:ss |])
      ==> (findMinLens `Compose` $(update [p| s:ss |] [p| s:ss |] [d| s = Replace; ss = sortLens |])),

      $(adaptiveSV [p| [] |] [p| v:vs |])
      ==> \s v -> v
    ]

-- | A lens which rearranges the minimum as the first element.
findMinLens :: (Show a, Ord a) => BiGUL [a] [a]
findMinLens = findMinRearr `Compose` reassemble
    where findMinRearr :: (Show a, Ord a) => BiGUL [a] (a, ([a], [a]))
          findMinRearr = Case
              [ $(normal [| \(s:ss) (v, ([], vs)) -> s == minimum (s:ss) |]
                         [| \(s:ss) -> s == minimum (s:ss) |])
                ==> $(update [p| s:ss |] [p| (s, ([], ss)) |]
                             [d| s = Replace; ss = Replace |]),

                $(adaptive [| \(s:ss) (_, (p:ps, _)) -> s == minimum (s:ss) |])
                ==> \s (_, (p:ps, _)) -> (p:s),

                $(adaptive [| \(s:ss) (_, ([], _)) -> True |])
                ==> \s v -> dropWhile (\k -> k /= minimum s) s,

                $(normal [| \s v -> True |] [| \s -> True |])
                ==> $(rearrV [| \(v, (p:ps, vs)) -> (p, (v, (ps, vs))) |])
                    $(update [p| s:ss |] [p| (s, ss) |]
                             [d| s = Replace; ss = findMinRearr |])
              ]

          reassemble :: (Show a) => BiGUL (a, ([a], [a])) [a]
          reassemble = $(update [p| (s, ss) |] [p| s:ss |] [d| s = Replace; ss = appendLens |])

-- | A lens which appends two lists. For the put-direction, it updates each element in the first 
-- list of the source first and then uses the rest of the view to update the second list.
appendLens :: (Show a) => BiGUL ([a], [a]) [a]
appendLens = Case [
    $(normalSV [p| ([],[]) |] [p| [] |] [p| ([],[]) |])
    ==> $(rearrV [| \[] -> () |]) (Skip (const ())),

    $(adaptiveSV [p| _ |] [p| [] |])
    ==> \s v -> ([], []),

    $(normalSV [p| (a:as,b) |] [p| v:vs |] [p| (a:as,b) |])
    ==> $(rearrS [| \(a:as,b) -> (a, (as,b)) |])
        $(update [p| (a, rest) |] [p| a:rest |] [d| a = Replace; rest = appendLens |]),

    $(normalSV [p| ([],b:bs) |] [p| v:vs |] [p| ([],b:bs) |])
    ==> $(rearrS [| \([],b:bs) -> (b, ([],bs)) |])
        $(update [p| (a, rest) |] [p| a:rest |] [d| a = Replace; rest = appendLens |]),

    $(adaptiveSV [p| ([],[]) |] [p| v:vs |])
    ==> \s v -> ([], v)
    ]

filterLens' p = emb (filter p) (filterPut p)
filterPut p s v = 
    if null (filter (not . p) v) 
       then filterPut' s v
       else (error "filterLens: invalid view")
    where filterPut' s [] = filter (not . p) s
          filterPut' [] v = v
          filterPut' (s:ss) (v:vs) = if not (p s) then s : (filterPut' ss (v:vs)) else (v : filterPut' ss vs)

bx1 :: BiGUL (Int, String) Int
bx1 = $(update [p| (x, _) |] [p| x |] [d| x = Replace |])

minEditDistLens :: (Show s, Show v, Eq s, Eq v) => BiGUL s v -> (v -> s) -> BiGUL [s] [v]
minEditDistLens bx create = Case
    -- 'bx' tries to update from view to source by simply replacing elements in source and view one by one,
    -- If it is not the most efficient way, we try to add elements or delete elements.
    [ $(adaptive [| \ s v -> length s /= length v || structureEdit bx create s v /= s |])
      ==> structureEdit bx create
    ,
     $(normalSV [p| _ |] [p| _ |] [p| _ |])
      ==> mapLens bx create
    ]

data Operation = OpModify | OpInsert | OpDelete | OpNothing deriving (Show, Eq, Ord)

minEditDistDP :: (Eq a) => [a] -> [a] -> Array (Int, Int) (Int, Operation)
minEditDistDP s v = runST $ do
    let (lenS, lenV) = (length s, length v)
    arrS <- newSTListArray (1, lenS) s
    arrV <- newSTListArray (1, lenV) v
    f <- newArray ((0,0), (lenS, lenV)) (-1, OpInsert) :: ST s (STArray s (Int, Int) (Int, Operation))
    forM_ [0 .. lenV] (\i -> writeArray f (0, i) (i, OpInsert))
    forM_ [0 .. lenS] (\i -> writeArray f (i, 0) (i, OpDelete))
    writeArray f (0,0) (0, OpNothing)
    forM_ (range ((1,1), (lenS, lenV))) (\(i, j) -> do
        si <- readArray arrS i
        sj <- readArray arrV j
        if si == sj 
           then readArray f (i-1, j-1) >>= \p -> writeArray f (i, j) (modifySnd OpNothing p)
           else do
               cModify <- readArray f (i-1, j-1)
               cInsert <- readArray f (i, j-1)
               cDelete <- readArray f (i-1, j)
               writeArray f (i, j) ((\(x, y) -> (x+1, y)) (minimum [ modifySnd OpModify cModify,
                                                                     modifySnd OpInsert cInsert,
                                                                     modifySnd OpDelete cDelete ]) ))
    freeze f
    where newSTListArray :: (Ix i) => (i,i) -> [a] -> ST s (STArray s i a)
          newSTListArray = newListArray
          modifySnd x (a, b) = (a, x)

structureEdit :: (Show s, Show v, Eq s, Eq v) => BiGUL s v -> (v -> s) -> [s] -> [v] -> [s]
structureEdit bx create s v = let sv = get (mapLens bx create) s
                              in if sv == Nothing 
                                    then s
                                    else let f = minEditDistDP (fromJust sv) v
                                             reconstruct 0 0 [] = []
                                             reconstruct i j s = case snd (f ! (i,j)) of
                                                 OpNothing -> head s : reconstruct (i-1) (j-1) (tail s)
                                                 OpModify -> head s : reconstruct (i-1) (j-1) (tail s)
                                                 OpInsert -> create (head v) : reconstruct i (j-1) s
                                                 -- FIXME: the following maybe more safe
                                                 -- OpInsert -> fromJust (put bx (create v) v) : reconstruct i (j-1) s
                                                 OpDelete -> reconstruct (i-1) j (tail s)
                                         in reverse $ reconstruct (length s) (length v) (reverse s)
