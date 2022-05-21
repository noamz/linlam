-- implements Wenjie Fang's bijection from "Bijections between planar
-- maps and planar linear normal λ-terms with connectivity condition"
-- (https://arxiv.org/abs/2202.03542)

module LinLam.WF where

import LinLam.Core
import LinLam.Cartes
import LinLam.Pretty
import LinLam.Utils

import qualified Math.Combinat.Permutations as P

import Data.List
import Data.Maybe
import qualified Data.Set as Set

-- A "one-corner component" (in the terminology of Fang's paper) is a
-- rooted planar map whose root corner is the only corner of the root
-- vertex on the outer face.

-- the "unPi" operation takes an integer k and a map m and constructs
-- a one-corner component by walking k vertices along the root face
-- and adding a new edge (see Fig 5 of Fang's paper)
unPi :: Int -> Carte -> Carte
unPi k m
  | m == trivialMap && k == 0 = loopMap
  | otherwise                 = Carte { ndarts = ndarts', sigma = sigma', alpha = alpha' }
  where
    -- the new map has one new edge = two new darts
    ndarts' = ndarts m + 2
    -- compute the list of vertices on the original root face, without duplicates
    vertices = reverse $ foldl (\vs x -> if any (elem x) vs then vs else (orbit x (act $ sigma m)):vs) [] (orbit (root m) (act $ phi m))
    -- choose the new target root vertex
    target = (vertices ++ [[ndarts'-1]]) !! k
    -- define the new vertex permutation and edge involution
    sigma' = P.toPermutation $ (head target+1 : map (\x -> if x == head target then ndarts' else x+1) (P.fromPermutation (sigma m)) ++ [1])
    alpha' = (P.transposition ndarts' (2,ndarts')) `P.multiplyPermutation`
             (P.toPermutation [1] `P.concatPermutations` alpha m `P.concatPermutations` P.toPermutation [1]) 

-- We define the bijection from normal planar terms to rooted planar maps
-- slightly differently but equivalently to Fang's paper, in terms of
-- two mutually recursive functions neutralToM and normalToU, which
-- respectively transform neutral and normal planar lambda terms into
-- rooted planar maps and into one-corner components.
neutralToM, normalToU :: LT -> Carte
neutralToM (V _)     = trivialMap
neutralToM (A t1 t2) = Carte { ndarts = ndarts', sigma = sigma', alpha = alpha' }
  where
    m = neutralToM t1
    u = normalToU  t2
    ndarts' = ndarts m + ndarts u - 1
    r1 = act (sigma m) (root m)
    r2 = act (sigma u) (root u) + ndarts m - 1
    _:s2 = map (\x -> if x == 1 then r1 else x+ndarts m-1) $ P.fromPermutation (sigma u)
    _:s1 = P.fromPermutation (sigma m)
    sigma' = P.toPermutation $ (r2 : s1 ++ s2)
    tailP = P.toPermutation . map pred . tail . P.fromPermutation
    alpha' = alpha m `P.concatPermutations` tailP (alpha u)
normalToU t1 = unPi k m
  where
    (xs,t2) = unlambdas t1
    m = neutralToM t2
    k = arity t2 - length xs

-- The bijection from normal planar terms to general rooted planar maps
-- is then defined by applying neutralToM to the underlying neutral body
-- after removing the outermost lambdas.
nptToMap = neutralToM . snd . unlambdas

-- Check that the image of nptToMap is the set of rooted planar maps with n edges
_test3 :: Int -> Bool
_test3 n = length ms == length ts &&
           all ((==1) . euler) ms
  where
    ts = allNPT (3*n+2) 0
    ms = nub $ map (canonifyCarte . nptToMap) ts

-- verified _test3 n for n <- [0..6]

-- Check that the image of nptToMap on second-order terms is the set of rooted planar trees with n edges
_test4 :: Int -> Bool
_test4 n = length ms == length ts &&
           all ((==1) . euler) ms &&
           all (\m -> length (orbit (root m) (act $ phi m)) == ndarts m) ms
  where
    ts = allNPT (2*n-1) n
    ms = nub $ map (canonifyCarte . nptToMap) ts

-- verified _test4 n for n <- [0..10]

-- Now we define the inverse bijection.

-- First we define the "doPi" operation, which is inverse to unPi.  It
-- takes a one-corner component u and returns a pair (k,m) an integer
-- k and a map m such that u = unPi k m.
doPi :: Carte -> (Int,Carte)
doPi u
  | u == loopMap = (0,trivialMap)
  | otherwise    = (k,m)
  where
    -- determine the edge clockwise from the root
    x = act (P.inversePermutation (sigma u)) (root u)
    x' = act (alpha u) x
    -- ...and remove it together with the original root dart
    m = dartsDelete u [1,x] x'
    k = head [i | i <- [0..], unPi i m `carteEquiv` u]

-- Next we define inverses to neutralToM and normalToU.  mToNeutral
-- expects a map and returns the corresponding neutral planar term,
-- while uToNormal expects a one-corner component and returns the
-- corresponding normal planar term.
mToNeutral, uToNormal :: Carte -> LT
mToNeutral m
  | m == trivialMap = V 0
  | otherwise       = A t1 t2
  where
    -- We remove the first one-corner component from the root vertex
    root_vertex = orbit (root m) (act $ sigma m)
    root_face   = orbit (root m) (act $ phi m)
    x = root_vertex !! 1
    x' = act (alpha m) x
    (rv1,rv2) = break (\y -> elem y root_face) (drop 2 root_vertex)
    y = if null rv1 then x else last rv1
    component = dfsCarte' m [root m] (Set.fromList rv2)
    u1 = Carte { ndarts = length component,
                 sigma = P.toPermutation [renameBy component (if i == y then 1 else act (sigma m) i) | i <- component],
                 alpha = P.toPermutation [renameBy component (act (alpha m) i) | i <- component] }
    z = if null rv2 then 1 else head rv2
    leftover = 1 : dfsCarte' m rv2 (Set.fromList component)
    m2 = Carte { ndarts = length leftover,
                 sigma = P.toPermutation [renameBy leftover (if i == 1 then z else act (sigma m) i) | i <- leftover],
                 alpha = P.toPermutation [renameBy leftover (act (alpha m) i) | i <- leftover] }
    t1 = mToNeutral m2
    t2 = shift t1 (uToNormal u1)

    renameBy :: [Int] -> Int -> Int
    renameBy xs i = fromJust (lookup i $ zip xs [1..])

uToNormal u = foldr L t [x | x <- drop k (free t)]
  where
    (k,m) = doPi u
    t = mToNeutral m

-- Finally we define an inverse to nptToMap by composing with closure
mapToNPT :: Carte -> LT
mapToNPT = closure . mToNeutral

-- test that mapToNPT is left inverse to nptToMap
_test5 :: Int -> Bool
_test5 n = all (\t -> mapToNPT (nptToMap t) == t) (allNPT (3*n+2) 0)

-- verified _test5 n for n <- [0..6]

-- transport along the bijection to compute the dual of a normal planar term
dualNPT :: LT -> LT
dualNPT = mapToNPT . dualMap . nptToMap
