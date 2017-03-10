module Model(module Model) where

data Expr = Expr (Maybe String) Expr'
data Expr' = Stop
           | Perm [Expr] [Expr]
           | Seq [Expr]
           | Noop [Expr]
           | Name String
type Context = [Expr]


forward :: Expr -> Expr
forward e@(Expr name (Seq s))
  | unify'' s s' = e
  | otherwise    = Expr Nothing (Seq s')
  where
    p:ps = s
    s' = forward' p ps
    forward' p@(Expr _ (Perm xs xs')) ps = permute xs xs' ps ++ [p]
    forward' p ps = p:ps
forward x = x

backward :: Expr -> Expr
backward e@(Expr name (Seq s))
  | unify'' s s' = e
  | otherwise    = Expr Nothing (Seq s')
  where
    p = last s
    ps = init s
    s' = backward' p ps
    backward' p@(Expr _ (Perm xs' xs)) ps = p : permute xs xs' ps
    backward' p ps = p:ps
backward x = x

permute _ _ ps = ps

-- hack : unify if names are equal, but could name two objects the same!
-- should really introduce idea of a context/environment...
unify (Expr (Just x) x') (Expr (Just y) y') = (x == y) || (unify' x' y')
unify (Expr _ x) (Expr _ y) = unify' x y

unify' Stop Stop = True
unify' (Perm xs xs') (Perm ys ys') = (unify'' xs ys) && (unify'' xs' ys')
unify' (Seq xs) (Seq ys) = unify'' xs ys
unify' (Noop xs) (Noop ys) = unify'' xs ys
unify' (Noop xs) (Seq ys) = (unify zh stop) && (unify'' xs zi) && (unify zl stop)
  where
    stop = Expr Nothing Stop
    zh:zt = ys
    zi = init zt
    zl = last zt
unify' x@(Seq _) y@(Noop _) = unify' y x
unify' (Name x) (Name y) = x == y
unify' _ _ = False

unify'' (x:xs) (y:ys) = (unify x y) && unify'' xs ys
unify'' [] [] = True
unify'' _ _ = False
