{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Module
( Aliasable(..)
, aliasig
, emptyContext
, compile
, incompile
, contextualise
, Context(..)
, CtxException
, Group(..)
) where

import Data.Maybe (isNothing, isJust, catMaybes, fromJust)
import Data.List (group, filter, sort, nub, foldl', partition)
import Data.Map.Lazy (Map, union, keys, elems, intersection, fromList)
import qualified Data.Map.Lazy as M

(!?) :: Ord k => Map k a -> k -> Maybe a
(!?) = flip M.lookup

mapKeyVals f = M.fromList . map (\(k,v) -> f k v) . M.toList

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys

--- deäliasing machinery

class Aliasable a where
    type Sig a :: *
    sig :: a b -> Sig a
    slots :: a b -> [b]
    reslot :: (r -> s) -> a r -> a s
    slotp :: a b -> Maybe b
    slotp _ = Nothing

type AOSO a r = (Aliasable a, Ord (Sig a), Ord r)

-- map each slot to its symbol signature, or Nothing if the symbol is undefined
resig :: Ord r => [([r], s)] -> [([r], Maybe [s])]
resig defs' = map resig' defs'
  where
    m = fromList $ map (\(l:_, s) -> (l, s)) defs'
    resig' (rs, _) = (rs, sequence $ map (m !?) rs)

-- group symbols by signature, leaving Nothings distinct
segregate :: Ord ss => [(rs, Maybe ss)] -> [[rs]]
segregate rs = map snd ok ++ (concatMap (map (:[]) . snd) err)
  where
    grouped = M.toList . M.fromListWith (++) $ map (\(x,y) -> (y,[x])) rs
    (ok, err) = partition (isJust . fst) grouped

-- create new signatures based on groups
regroup :: [[rs]] -> [(rs, Int)]
regroup = concat . zipWith (map . flip (,)) [1..]

-- erase slot information
aliases :: Ord r => [[[r]]] -> [[r]]
aliases = sort . map sort . map (map head)

-- group a symbol table into semantically equivalent symbols
groupAliases :: AOSO a r => [(r, a r)] -> [[r]]
groupAliases = converge (==) . map aliases . iterate iter . prep
  where
    prep = segregate . map (\(l, f) -> (l : slots f, Just $ sig f))
    iter = segregate . resig . regroup

-- resolve ref (slot) chains into their final expressions
resolveSlotSlots :: (Aliasable a, Ord r) => Map r (a r) -> Map r (a r)
resolveSlotSlots = uncurry (union . M.map fst)
                 . until (M.null . unclosed . fst) iterclose
                 . (\(rm,dm) -> (M.map (fromJust <$>) rm, M.map fst dm))
                 . M.partition (isJust . snd)
                 . M.map (\e -> (e, slotp e))

iterclose (rm,dm) = M.foldrWithKey transfer (rm,dm) $ unclosed rm
unclosed rm = transpose rm `M.difference` rm
transpose = mapKeyVals (\s (e,r) -> (r,(e,s)))
transfer r (e,s) (rm,dm) = (M.delete s rm,
                            M.insert s (M.findWithDefault e r dm) dm)

-- lookup generalised signature from alias map
gsigLookup :: (Aliasable a, Ord r) => Map r n -> a r -> (Sig a, [Either r n])
gsigLookup m = let deref ref = maybe (Left ref) Right $ M.lookup ref m
               in (\e -> (sig e, slots e)) . reslot deref

-- create a unique signature for an expression from a symbol table
aliasig :: AOSO a r => Map r (a r) -> a r -> (Sig a, [Either r Int])
aliasig m = let m' = resolveSlotSlots m
                sigm = gsigLookup . fromList . regroup
                                  . groupAliases $ M.toList m'
                refm r d = M.findWithDefault d r $ M.map sigm m'
            in \e -> maybe id refm (slotp e) (sigm e)

aliasig' :: AOSO a r => Map r (a r) -> [[r]]
aliasig' = groupAliases . M.toList . resolveSlotSlots

--- type definitions

data Group e r = Group { defs :: [(r, e r)]
                       , children :: [Group e r]
                       , promotes :: [(r, r)] -- (x, y) => expose x as y
                       , ismod :: Bool
                       } deriving Show

data Context e r = Context { symbols :: Map (r, Int) (e (r, Int))
                           , exposed :: Map r (r, Int)
                           , doppels :: [[(r, Int)]]
                           , sigify :: e (r, Int) -> (Sig e, [Either (r,Int) Int])
                           , equivalentp :: e (r, Int) -> e (r, Int) -> Bool }

data CtxTemp e r = CtxTemp { syms :: Map (r, Int) (e (r, Maybe Int))
                           , expd :: Map r (r, Int) }

type CtxException = String

type CtxMaybe = Either CtxException

type TempContext e r = CtxMaybe (Int, CtxTemp e r)

type AOS e r = (Aliasable e, Ord r, Show r)

--- public functions

-- TODO: incompile shouldn't have to regenerate aliasig,
--       this should be recomputed incrementally

emptyContext :: AOSO e r => Context e r
emptyContext = toContext $ CtxTemp M.empty M.empty

compile :: (AOS e r, Ord (Sig e)) => Group e r -> CtxMaybe (Context e r)
compile g = toContext . snd <$> compile' 0 g

incompile :: (AOS e r, Ord (Sig e)) => Context e r -> Group e r -> CtxMaybe (Context e r)
incompile c g = fromContext c >>= incollate g >>= cap >>= return . toContext . snd

contextualise :: AOS e r => Context e r -> e r -> CtxMaybe (e (r, Int))
contextualise c e = let e' = reslot (resolutionMap (exposed c) . (,Nothing)) e
                    in if Nothing `elem` (map snd $ slots e')
                       then err . show . notfound $ slots e'
                       else Right $ reslot unwrapSym e'
  where err = Left . (++) "Evaluation error: couldn't resolve symbol(s): "

--- conversion functions

fromContext :: Aliasable e => Context e r -> TempContext e r
fromContext c = let c' = CtxTemp { syms = M.map (reslot wrapSym) $ symbols c
                                 , expd = exposed c }
                in return (safeNum c', c')

toContext :: AOSO e r => CtxTemp e r -> Context e r
toContext c = let syms' = M.map (reslot unwrapSym) $ syms c
                  sig' = aliasig syms'
              in Context { symbols = syms'
                         , exposed = expd c
                         , doppels = aliasig' syms'
                         , sigify = sig'
                         , equivalentp = \x y -> sig' x == sig' y }

safeNum :: Aliasable e => CtxTemp e r -> Int
safeNum c = succ . foldl' max 0 $ se c ++ ss c ++ sd c
  where
    se = map snd . elems . expd
    ss = map snd . keys . syms
    sd = catMaybes . map snd . concatMap slots . elems . syms

--- main compilation logic

compile' :: AOS e r => Int -> Group e r -> TempContext e r
compile' n g = foldr f (lift n g) (children g) >>= res
                >>= reexport (promotes g) >>= shine
  where
    f x z = z >>= collate x
    shine = if ismod g then cap else return
    res = return . (resolve <$>)

collate :: AOS e r => Group e r -> (Int, CtxTemp e r) -> TempContext e r
collate g (n, c) = compile' n g >>= combine c

combine :: (Ord r, Show r) => CtxTemp e r -> (Int, CtxTemp e r) -> TempContext e r
combine c (n', c') = let reexps = keys $ expd c `intersection` expd c'
                     in if not $ null reexps then cecf "import" reexps
                        else return (n', CtxTemp (syms c `union` syms c')
                                                 (expd c `union` expd c'))

--- incremental compilation

incompile' :: AOS e r => Int -> Group e r -> TempContext e r
incompile' n g = foldr (\x z -> z >>= collate x) (lift n g) (children g)

inrestore :: Ord r => CtxTemp e r -> CtxTemp e r -> CtxTemp e r
inrestore c c' = c' {expd = expd c' `union` expd c}

incollate :: AOS e r => Group e r -> (Int, CtxTemp e r) -> TempContext e r
incollate g (n, c) = incombine c <$> incompile' n g
                                 >>= reexport (promotes g)
                                 >>= return . (resolve . inrestore c <$>)

incombine :: (Ord r, Show r) => CtxTemp e r -> (Int, CtxTemp e r) -> (Int, CtxTemp e r)
incombine c (n', c') = (n', CtxTemp (syms c' `union` syms c) (expd c' `union` expd c))

--- compilation helper functions

lift :: AOS e r => Int -> Group e r -> TempContext e r
lift n g = safeFromList (cecf "definition" . map fst) (map (wrapDef n) $ defs g)
             >>= \ss -> return (n + 1, CtxTemp ss $ expSyms ss)

reexport :: (Ord r, Show r) => [(r,r)] -> (Int, CtxTemp e r) -> TempContext e r
reexport ps (n, c) = shadow ps (expd c) >>= safeFromList (cecf "export")
                                        >>= \e -> return (n, c { expd = e })

resolutionMap :: Ord r => Map r (r, Int) -> (r, Maybe Int) -> (r, Maybe Int)
resolutionMap e l@(s, Nothing) = maybe l wrapSym (M.lookup s e)
resolutionMap _ l = l

resolve :: (Aliasable e, Ord r) => CtxTemp e r -> CtxTemp e r
resolve c = c { syms = M.map (reslot . resolutionMap $ expd c) (syms c) }

cap :: AOS e r => (Int, CtxTemp e r) -> TempContext e r
cap (n, c) = (n,) <$> let u = notfound . concatMap slots . elems $ syms c
                      in if null u then return c else cerr $ msg ++ show u
  where msg = "couldn't resolve symbol(s): "

shadow :: (Show r, Ord r) => [(r,r)] -> Map r (r, Int) -> CtxMaybe [(r, (r, Int))]
shadow [] _         = Right []
shadow ((x,y):rs) m = case M.lookup x m of
                        Just z  -> shadow rs m >>= return . (:) (y,z)
                        Nothing -> cerr $ "Can't find symbol to export: " ++ show x

--- exceptional and miscellaneous helper functions

cerr :: String -> CtxMaybe a
cerr = Left . ("Compilation error: " ++)

cecf :: Show t => String -> t -> CtxMaybe a
cecf t = cerr . (++) ("competing " ++ t ++ "(s) for: ") . show

expSyms = fromList . map (\s -> (fst s, s)) . keys
wrapDef n (s, d) = ((s, n), reslot (\r -> (r, Nothing)) d)
wrapSym (s, n) = (s, Just n)
unwrapSym (s, Just n) = (s, n)

excesses :: Ord a => [a] -> [a]
excesses = map head . filter ((> 1) . length) . group . sort

safeFromList :: Ord k => ([k] -> CtxMaybe (Map k v)) -> [(k,v)] -> CtxMaybe (Map k v)
safeFromList errf kvs = let zs = excesses $ map fst kvs
                        in case zs of
                             [] -> Right $ fromList kvs
                             _  -> errf zs

notfound :: Ord a => [(a, Maybe b)] -> [a]
notfound = nub . map fst . filter (isNothing . snd)