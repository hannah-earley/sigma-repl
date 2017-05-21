{-# LANGUAGE QuasiQuotes #-}

module Overture
( overture
) where

import qualified Graph as G
import qualified Input as I
import Text.RawString.QQ (r)

overture :: IO G.Graph
overture = do g <- G.empty
              g' <- I.loadRaw g overture'
              return $ g' { G.overture = G.root g' }

overture' :: String
overture' = [r|

  (perm (~1 [f1 . fs] z g) (f1 [g . fs] z ~1))
  (perm (~2 [f1 f2 . fs] z g) (f2 [f1 g . fs] z ~2))

  (* list literals *)
    (beq ~1@nil)
    (beq ~2 @cons)

  (* nat literals *)
    (beq ~1@zero)
    (beq ~2@succ)

|]
