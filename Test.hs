{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Scope2
import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified Data.Map.Lazy as Map

--- scopes

instance {-# OVERLAPS #-} (Show r, Show d) => Show (Context r d) where
  show (s, cs, r, d) = "<" ++ e ++ " | " ++ c ++ " | "
                           ++ show r ++ " -> " ++ show d ++ ">\n"
    where
      e = show $ exposed s
      c = intercalate ":" (cs ++ [show r])

dl = defines . Map.fromListWith error

ws = dl [(1,"a"),(2,"b"),(3,"c"),(41,"e")]
xs = dl [(4,"d"),(5,"e"),(6,"f")]
ys = dl [(7,"g"),(8,"h"),(9,"i")]
zs = dl [(21,"j"),(22,"k"),(23,"l")]

g = fromJust $ group [ws,zs]
h = fromJust $ group [xs,ys]
i = fromJust $ group [h, ws, p, j, protect zs]

--private :: Ord r => Scope r d -> [r] -> Scope r d
private s rs = restrict s (Map.fromList $ zip rs rs)

p = private (dl [(1,"p"),(10,"q"), (11, "r"), (12,"z")]) [10,11]
q = restrict (dl [(1,"m"),(2,"n"),(3,"o")])
          $ Map.fromList [(13,1),(14,2),(17,3)]

j = protect q
k = file "k.sig" (Map.fromList $ zip [0..6] [1..7]) i

go :: Ord r => Maybe (Context r d) -> r -> Maybe (Context r d)
go (Just (s, _, _, _)) = resolve s
go Nothing = const Nothing