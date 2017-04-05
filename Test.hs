import Scope
import Data.List (intercalate)
import qualified Data.Map.Lazy as Map

--- scopes

instance Show r => Show (Context r d) where
  show = intercalate " > " . reverse . ctxTrail

dl :: Ord r => [(r,d)] -> Scopy r d
dl = MkScopy . DefList . Map.fromListWith (++) . map (pure <$>)

ws = dl [(1,"a"),(2,"b"),(3,"c"),(4,"e")]
xs = dl [(4,"d"),(5,"e"),(6,"f")]
ys = dl [(7,"g"),(8,"h"),(9,"i")]
zs = dl [(1,"j"),(1,"jj"),(2,"k"),(3,"l")]

g = ScopeGroup [ws,zs]
h = ScopeGroup [xs,ys]
i = ScopeGroup $ [MkScopy h, ws, j, MkScopy p, MkScopy q]

private :: (Scope s, Ord r) => s r d -> [r] -> Restricted r d
private s rs = Restricted s (Map.fromList $ zip rs rs)

p = private (dl [(1,"p"),(1,"pp"),(10,"q"), (11, "r"), (12,"z")]) [1,11]
q = Restricted (dl [(1,"m"),(2,"nn"),(2,"n"),(3,"o")]) $
          Map.fromList [(13,1),(14,2),(7,3)]

j = MkScopy . Insulated $ ScopeGroup [zs,zs]
k = MkScopy $ mkFile "k.sig" (Map.fromList $ zip [0..6] [1..7]) i

go :: Ord r => Int -> [(d, Context r d)] -> r -> [(d, Context r d)]
go n = get . snd . (!!n)

---