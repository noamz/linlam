-- conversion between linear lambda terms and 3-valent maps
module LinLam.Trivalent (toLT) where

import Data.List
import Data.Maybe
import qualified Data.Set as Set

import LinLam.Core
import LinLam.Cartes

-- auxiliary function to convert a rooted 3-valent map to a linear lambda term,
-- taking as extra parameter a specified root and sets of visited half-edges
-- and boundary half-edges, and returning a term together with an updated
-- visited set
toLT' :: Carte -> Int -> Set.Set Int -> Set.Set Int -> (LT, Set.Set Int)
toLT' m r visited boundary
  | Set.member (act (alpha m) r) boundary = (V r, Set.insert r visited)
  | otherwise =
    let r1 = act (sigma m) (act (alpha m) r) in
    let r2 = act (sigma m) (act (sigma m) (act (alpha m) r)) in
    let (t1,vis') = toLT' m r1 (Set.insert r visited) (Set.insert r2 boundary) in
    if Set.member (act (alpha m) r2) vis' then
      (L (act (alpha m) r2) t1, vis')
    else
      let (t2,vis'') = toLT' m r2 vis' boundary in
      (A t1 t2, vis'')

-- convert a rooted 3-valent map to a linear lambda term
toLT :: Carte -> LT
toLT m = let (t,_) = toLT' m (root m) Set.empty Set.empty in t

{-
-- convert a linear lambda term to a rooted 3-valent map
fromLT :: LT -> Carte
fromLT t = Carte { ndarts = ndarts , sigma = sigma , alpha = alpha }
  where
    t' = canonify t
    kus = focus t'
    binder :: Int -> Maybe (LTdot,LT)
    binder x = find (\(k,u) -> isLam u && let L y _ = u in x == y) kus
    edge :: (LTdot,LT) -> [(LTdot,LT,EDit)]
    edge (k,u) = [(k,u,b) | b <- False : [True | not (isVar u) || (isVar u && let V x = u in not (x `elem` free t'))]]
    ds = map edge kus
    sigma_u :: (LTdot,LT,Bool) -> (LT,Bool)
    sigma_u (k,u@(L x u'),True) = (x,True)
    sigma_u (u@(L x u'),False) = 
    ndarts = length ds
-}

  
