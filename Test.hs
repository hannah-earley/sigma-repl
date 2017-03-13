module Test(module Test) where

import Model
import Numeric.Natural

l = Label
r = Ref

d xs = Seq ([Stop] ++ xs ++ [Stop])

-- halts
top = Stop
bot = Stop
stp = Stop

-- choices
choice :: Natural -> Expr
choice m = Perm [pattern "f" m, l"z", l"g"]
                [l"f", pattern "g" m, l"z"]
  where
    pattern :: String -> Natural -> Expr
    pattern f 0 = cons (l f) (l"fs")
    pattern f n = cons (l ('f' : show n)) (pattern f (n-1))

primFst = Perm
  [d[r"2p", d[l"f", l"fs"]], l"z", l"g"]
  [l"f", d[r"2p", d[l"g", l"fs"]], l"z"]

primSnd = Perm
  [d[r"2p", d[l"f", d[r"2p", d[l"g", l"gs"]]]], l"z", l"h"]
  [l"g", d[r"2p", d[l"f", d[r"2p", d[l"h", l"gs"]]]], l"z"]

-- nats
zero = choice 0
suc = choice 1

nat :: Natural -> Expr
nat 0 = d[r"zero", stp]
nat n = d[r"succ", nat (n-1)]

-- lists
cnil = choice 0
ccons = choice 1
nil = d[r"nil", stp]
cons a b = d[r"cons", d[a, b]]
lst [] = nil
lst (x:xs) = cons x (lst xs)

-- list reversal
rev = Perm
  [r"reverse'", l"l", top]
  [r"nil", lst[r"rev-loop", r"rev-next"], d[stp, l"l"]]

rev' = Perm
  [lst[r"rev-loop", r"rev-next"], d[l"r", stp], r"nil"]
  [bot, l"r", r"reverse"]

loop = Perm
  [lst[r"reverse", r"next"], d[l"r'", d[l"l?", l"l'"]], l"r?"]
  [l"l?", lst[r"reverse'", r"next"], d[d[l"r?", l"r'"], l"l'"]]

next = Perm
  [lst[r"reverse'", r"next"], d[l"r", d[l"x", l"xs"]], r"cons"]
  [r"cons", lst[r"reverse", r"loop"], d[d[l"x", l"r"], l"xs"]]



lib = [ ("top", top), ("bottom", bot), ("stop", stp)
      , ("1p", primFst), ("2p", primSnd)
      , ("1o", choice 0), ("2o", choice 1), ("3o", choice 2), ("4o", choice 3)
      , ("zero", zero), ("succ", suc), ("n0", nat 0), ("n1", nat 1), ("n2", nat 2), ("n3", nat 3)
      , ("nil", cnil), ("cons", ccons)
      , ("reverse", rev), ("reverse'", rev'), ("rev-loop", loop), ("rev-next", next) ]
