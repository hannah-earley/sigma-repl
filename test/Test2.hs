module Test2 (module Test2) where

import Data.Set (fromList)
import Representation (dedup)
import Common (Partition, partition, pmap)

prep :: (Ord a, Ord r) => [(a,(r,[r]))] -> Partition (r,[r])
prep = pmap snd . partition fst . fromList

dd :: (Ord a, Ord r) => [(a,(r,[r]))] -> Partition r
dd = dedup . prep

l = [("a", (11, [21, 12, 31]))
    ,("a", (12, [22, 13, 32]))
    ,("a", (13, [22, 13, 32]))
    ,("a", (21, [13, 41, 31]))
    ,("a", (22, [11, 41, 32]))
    ,("c", (31, [42]))
    ,("c", (32, [42]))
    ,("d", (41, []))
    ,("d", (42, []))]

m = [("a", (1, [2,1,3]))
    ,("a", (2, [1,4,3]))
    ,("c", (3, [4]))
    ,("d", (4, []))]
