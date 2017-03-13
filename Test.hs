{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Test(module Test) where

import Module
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

---------------------
-- test deäliasing --
---------------------

data Fun = Fun { fun_sig :: String
               , fun_slots :: [ID]
               } deriving (Show)

instance Aliasable Fun where
    type Sig Fun = String
    type Ref Fun = ID
    sig = fun_sig
    slots = fun_slots


funs = [ ("x", Fun "b" ["y", "f"])
       , ("y", Fun "b" ["y", "g"])
       , ("f", Fun "a" ["x", "f"])
       , ("g", Fun "a" ["y", "f"])
       , ("h", Fun "a" ["x", "y"])
       , ("i", Fun "a" ["y", "h"]) ]

funs' = [ ("f", Fun "a" ["g", "f"])
        , ("g", Fun "b" ["h", "f"])
        , ("h", Fun "b" ["h", "i"])
        , ("i", Fun "a" ["h", "f"])
        , ("j", Fun "a" ["g", "h"]) ]

funs'' = [ ("f",Fun"a"["g","h"])
         , ("g",Fun"a"["f","i"])
         , ("h",Fun"b"["h","h"])
         , ("i",Fun"b"["h","i"]) ]

funs''' = [ ("1º", Fun "1" ["2º", "2p"])
          , ("nil", Fun "1" ["cons", "2p"])
          , ("zero", Fun "1" ["succ", "2º"])
          , ("1p", Fun "1" ["2p", "2p"])

          , ("2º", Fun "2" ["2p", "2p"])
          , ("cons", Fun "2" ["succ", "succ"])
          , ("succ", Fun "2" ["cons", "2º"])
          , ("2p", Fun "2" ["2p", "2p"])

          , ("f", Fun "2" ["2º", "3º"])
          , ("3º", Fun "3" ["2p", "cons"]) ]
