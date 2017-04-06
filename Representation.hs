{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Representation
( module Representation
) where

import Common (Partition, repartition, index, pmap, converge)

distinguish :: Ord r => Partition (r, [r]) -> Partition (r, [r])
distinguish = repartition =<< (. snd) . map . index . pmap fst

dedup :: Ord r => Partition (r, [r]) -> Partition r
dedup = pmap fst . converge (==) distinguish
