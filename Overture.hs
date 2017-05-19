module Overture
( overture
) where

import qualified Input as I

overture :: IO I.Graph
overture = do g <- I.empty
              g' <- I.loadRaw g prelude
              return $ g' { I.overture = I.root g' }

prelude = "(perm (~1 [f1 . fs] z g) (f1 [g . fs] z ~1))\
          \(perm (~2 [f1 f2 . fs] z g) (f2 [f1 g . fs] z ~2))\
          \\
          \(* list literals *)\
          \(beq ~1@nil)\
          \(beq ~2 @cons)\
          \\
          \(* nat literals *)\
          \(beq ~1@zero)\
          \(beq ~2@succ)"
