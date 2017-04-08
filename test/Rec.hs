module Rec (module Rec) where

import qualified Data.Map.Lazy as M

data RoseTree a = RoseTree a [RoseTree a] deriving (Show)

exampleTree :: RoseTree Int
exampleTree = RoseTree 5 [RoseTree 4 [], RoseTree 6 []]

pureMax :: Ord a => RoseTree a -> RoseTree (a,a)
pureMax tree = t
  where
    (t, largest) = go tree
    -- go :: Ord a => RoseTree a -> (RoseTree (a,a), a)
    go (RoseTree x []) = (RoseTree (x,largest) [], x)
    go (RoseTree x xs) = let (xs', largests) = unzip $ map go xs
                         in (RoseTree (x,largest) xs', max x (maximum largests))

foo :: Int -> (Int -> a) -> [(Int, a)]
foo n f = map (\m -> (m, f m)) [n..]

bar :: Int -> [(Int,Maybe Int)]
bar n = let l = foo n f
            l' = zip (map fst l) [1..]
            g = sum . map fst . flip take l . (3*)
            f = fmap g . flip lookup l'
        in l

qux :: Int -> Int -> (Int -> a) -> M.Map Int (Int, a)
qux a b f = foldl (\m n -> M.insert n (n*n, f n) m) M.empty [a..b]

quux :: Int -> Int -> M.Map Int (Int, Int)
quux a b = let m = qux a b f
               l = M.toAscList m
               f n = sum . map (fst . snd) $ dropWhile ((<n) . fst) l
           in m
