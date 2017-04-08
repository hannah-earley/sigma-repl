{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Representation
( module Representation
) where

import Common (Partition, subpartition, index, pmap, converge)
import qualified Data.Map.Lazy as M

distinguish :: Ord r => Partition (r, [r]) -> Partition (r, [r])
distinguish = subpartition =<< (. snd) . map . (M.!) . index . pmap fst

dedup :: Ord r => Partition (r, [r]) -> Partition r
dedup = pmap fst . converge (==) distinguish
