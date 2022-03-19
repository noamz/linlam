-- conversion between linear lambda terms and 3-valent maps
module LinLam.Trivalent (toLT, fromLT) where

import Data.List
import Data.Maybe
import qualified Data.Set as Set

import qualified Math.Combinat.Permutations as P

import LinLam.Core
import LinLam.Cartes

import Debug.Trace

-- auxiliary function to convert a rooted 3-valent map to a linear lambda term,
-- taking as extra parameter a specified root and sets of visited half-edges
-- and boundary half-edges, and returning a term together with an updated
-- visited set
toLT' :: Carte -> Int -> Set.Set Int -> Set.Set Int -> (LT, Set.Set Int)
toLT' m r visited boundary
  | Set.member (act (alpha m) r) boundary = (V r, Set.insert r visited)
  | otherwise =
    let r2 = act (sigma m) (act (alpha m) r) in
    let r1 = act (sigma m) r2 in
    let (t1,vis') = toLT' m r1 (Set.insert r visited) (Set.insert r2 boundary) in
    if Set.member (act (alpha m) r2) vis' then
      (L (act (alpha m) r2) t1, vis')
    else
      let (t2,vis'') = toLT' m r2 vis' boundary in
      (A t1 t2, vis'')

-- convert a rooted 3-valent map to a linear lambda term
toLT :: Carte -> LT
toLT m = let (t,_) = toLT' m (root m) Set.empty Set.empty in t

data Dir = Up | Dn
  deriving (Show,Eq)
type Dart = (LTdot,LT,Dir)

-- convert a linear lambda term to a rooted 3-valent map
fromLT :: LT -> Carte
fromLT t = Carte { ndarts = ndarts , sigma = sigma , alpha = alpha }
  where
    t' = canonify t
    kus = focus t'
    binder :: Int -> Maybe (LTdot,LT)
    binder x = find (\(k,u) -> isLam u && let L y _ = u in x == y) kus

    alphau :: Dart -> Dart
    alphau (k,t,Up)    = (k,t,Dn)
    alphau (k,V x,Dn)  = if x `elem` free t' then (k,V x,Dn) else (k,V x,Up)
    alphau (k,t,Dn)    = (k,t,Up)
    
    sigmau :: Dart -> Dart
    sigmau (k, A t1 t2, Up) = (A'2 t1 k, t2, Dn)
    sigmau (A'2 t1 k,t2,Dn) = (A'1 k t2, t1, Dn)
    sigmau (A'1 k t2,t1,Dn) = (k, A t1 t2, Up)
    sigmau (k,L x t, Up)    = (catcxt k' (L' x k), V x, Up)
      where k' = head (focusVar t x)
    sigmau (k0,V x, Up)     = (L' x k, plug k' (V x), Dn)
      where
        (k,k') = splitCxtAt x k0 Hole
        splitCxtAt x (L' y k)   k' = if x == y then (k,revcxt k') else splitCxtAt x k (L' y k')
        splitCxtAt x (A'1 k t2) k' = splitCxtAt x k (A'1 k' t2)
        splitCxtAt x (A'2 t2 k) k' = splitCxtAt x k (A'2 t2 k')
    sigmau (L' x k,t, Dn)   = (k, L x t, Up)
    sigmau (k,t,Dn)      = (k,t,Dn)

    edge :: (LTdot,LT) -> [Dart]
    edge (k,u) = [(k,u,b) | b <- Dn : [Up | not (isVar u) || (isVar u && let V x = u in not (x `elem` free t'))]]
    ds = concatMap edge kus
    labels = zip ds [1..]
    di d = fromJust $ lookup d labels
    ndarts = length ds
    sigma = P.toPermutation $ [di (sigmau d) | d <- ds]
    alpha = P.toPermutation $ [di (alphau d) | d <- ds]

