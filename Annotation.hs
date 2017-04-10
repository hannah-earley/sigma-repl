module Annotation
( module Annotation
) where

import Model (Expr(..), Perm, ID, Named(..), Hash, sig, slots)
import Scope (Scope, Context(ref, def), collect)
import Representation (hashify)
import Common (acyclicp)
import Control.Monad (ap, liftM2)

-- check that non-perm defs are acyclic, to ensure that
-- dereferencing will terminate
defcheck :: Scope ID Expr -> Bool
defcheck = acyclicp . map ((,,) `ap` ref `ap` (deps . def)) . collect
  where
    deps (ESeq (ByRef r) _) = [r]
    deps (ESeq _ es) = foldr ((++) . deps) [] es
    deps _ = []

deduperm :: [(Int, Perm)] -> [(Int, Hash)]
deduperm = hashify . map (liftM2 (,) sig slots <$>)
