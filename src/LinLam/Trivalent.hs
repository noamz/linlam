-- conversion between linear lambda terms and 3-valent maps
module LinLam.Trivalent where

import Data.List
import Data.Maybe
import qualified Data.Set as Set

import qualified Math.Combinat.Permutations as P

import LinLam.Core
import LinLam.Cartes
import LinLam.Utils

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

-- intrinsic representation of linear lambda terms as combinatorial maps

-- the darts of the map are "oriented subterms", i.e., pairs ((c,t),dir)
-- of a marked subterm c[t] and a direction up/down towards the root
data Dir = Up | Dn
  deriving (Show,Eq,Ord)

type IDart = (LTfoc,Dir)

data IMap = IMap { idarts :: [IDart],
                   isigma :: IDart -> IDart,
                   ialpha :: IDart -> IDart,
                   iphi'  :: IDart -> IDart }

oppdir :: Dir -> Dir
oppdir Up = Dn
oppdir Dn = Up

-- compute the intrinsic combinatorial map associated to a linear lambda term
intrinsicMap :: LT -> IMap
intrinsicMap t = IMap { idarts = idarts, isigma = isigma, ialpha = ialpha, iphi' = iphi' }
  where
    idarts = concatMap (\cu -> [(cu,Dn),(cu,Up)]) (focus t)

    ialpha (ot,eps) = (ot, oppdir eps)

    iphi' ((k, A t1 t2), Up)  = ((A'2 t1 k, t2), Up)
    iphi' ((A'2 t1 k, t2),Dn) = ((A'1 k t2, t1), Up)
    iphi' ((A'1 k t2, t1),Dn) = ((k, A t1 t2), Dn)
    iphi' ((k,L x t), Up)     = ((focusVar t x `catcxt` L' x k, V x), Dn)
    iphi' ((k0,V x), Up)      = case splitCxtAt x k0 Hole of
      Just (k,k') -> ((L' x k, plug k' (V x)), Up)
      Nothing     -> ((k0,V x), Dn)
      where
        splitCxtAt x (L' y k)   k' = if x == y then Just (k,revcxt k') else splitCxtAt x k (L' y k')
        splitCxtAt x (A'1 k t2) k' = splitCxtAt x k (A'1 k' t2)
        splitCxtAt x (A'2 t2 k) k' = splitCxtAt x k (A'2 t2 k')
        splitCxtAt x Hole       k' = Nothing
    iphi' ((L' x k,t), Dn)    = ((k,L x t), Dn)
    iphi' ((Hole,t),Dn)      = ((Hole,t),Up)

    isigma = ialpha . iphi'

-- convert a linear lambda term to an integer labeled rooted 3-valent map
fromLT :: LT -> Carte
fromLT t = Carte { ndarts = ndarts , sigma = sigma , alpha = alpha }
  where
    imap = intrinsicMap t
    labels = zip (idarts imap) [1..]
    di d = fromJust $ lookup d labels
    ndarts = length labels
    sigma = P.toPermutation $ [di (isigma imap d) | d <- idarts imap]
    alpha = P.toPermutation $ [di (ialpha imap d) | d <- idarts imap]

-- determine whether two oriented subterms are in the same face of a term
sameFace :: LT -> IDart -> IDart -> Bool
sameFace t ou1 ou2 = ou1 `elem` orbit ou2 (iphi' $ intrinsicMap t)

-- inductive computation of the genus
genusLT :: LT -> Int
genusLT (V _)     = 0
genusLT (A t1 t2) = genusLT t1 + genusLT t2
genusLT (L x t1)  = genusLT t1 + sf
  where
    sf :: Int
    sf | sameFace t1 ((Hole,t1),Dn) ((focusVar t1 x,V x),Dn) = 0
       | otherwise = 1
