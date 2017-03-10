module Test(module Test) where
import Model
import Numeric.Natural

-- shortcuts
n name = Expr (Just name)
u = Expr Nothing
r name (Expr _ expr) = n name expr
l = u . Name
d = u . Noop

-- halts
top = "Top" `n` Stop
bot = "Bottom" `n` Stop
stp = "Nothing" `n` Stop

-- choices
primFst = "0?" `n` Perm
  [d[l"f?", d[l"f", l"fs"]], l"z", l"g"]
  [l"f", d[l"f?", d[l"g", l"fs"]], l"z"]
primSnd = "0?" `n` Perm
  [d[l"f?", d[l"f", d[l"g?", d[l"g", l"gs"]]]], l"z", l"h"]
  [l"g", d[l"f?", d[l"f", d[l"g?", d[l"h", l"gs"]]]], l"z"]
first = choice 0
second = choice 1

choice :: Natural -> Expr
choice 0 = primFst
choice 1 = primSnd
choice m = (show m ++ "?") `n` Perm [pattern m, l"z", l"g"]
                                    [l ('f' : show m), qattern m, l"z"]
  where
    pattern :: Natural -> Expr
    pattern 0 = cons (l"f0") (l"fs")
    pattern n = cons (l ('f' : show n)) (pattern (n-1))

    qattern :: Natural -> Expr
    qattern 0 = cons (l"g") (l"fs")
    qattern n = cons (l"g") (pattern (n-1))

-- lists
cnil = r "Nil" first
ccons = r "Cons" second
nil = d[cnil, stp]
cons a b = d[ccons, d[a, b]]
lst [] = nil
lst (x:xs) = cons x (lst xs)

-- nats
zero = r "Zero" first
suc = r "Succ" second
nat :: Natural -> Expr
nat 0 = d[zero, stp]
nat n = d[suc, nat (n-1)]

-- list reversal
rev = "reverse" `n` Perm
  [rev', l"l", top]
  [cnil, lst[loop, next], d[stp, l"l"]]

rev' = "reverse'" `n` Perm
  [lst[loop, next], d[l"r", stp], cnil]
  [bot, l"r", rev]

loop = "loop" `n` Perm
  [lst[rev, next], d[l"r'", d[l"l?", l"l'"]], l"r?"]
  [l"l?", lst[rev', next], d[d[l"r?", l"r'"], l"l'"]]

next = "next" `n` Perm
  [lst[rev', next], d[l"r", d[l"x", l"xs"]], ccons]
  [ccons, lst[rev, loop], d[d[l"x", l"r"], l"xs"]]
