-- implements Wenjie Fang's bijection from "Bijections between planar
-- maps and planar linear normal λ-terms with connectivity condition"
-- (https://arxiv.org/abs/2202.03542)

module LinLam.WF where

import LinLam.Core
import LinLam.Cartes
import LinLam.Pretty

import qualified Math.Combinat.Permutations as P

import Data.List

-- A "one-corner component" (in the terminology of Fang's paper) is a
-- rooted planar map whose root corner is the only corner of the root
-- vertex on the outer face.

-- the "unPi" operation takes an integer k and a map m and constructs
-- a one-corner component by walking k vertices along the root face
-- and adding a new edge (see Fig 5 of Fang's paper)
unPi :: Int -> Carte -> Carte
unPi k m
  | k == 0 && m == trivialMap = Carte { ndarts = 3, sigma = P.toPermutation [3,1,2], alpha = P.toPermutation [1,3,2] }
  | otherwise                 = Carte { ndarts = ndarts', sigma = sigma', alpha = alpha' }
  where
    -- the new map has one new edge = two new darts
    ndarts' = ndarts m + 2
    -- compute the list of vertices on the original root face, without duplicates
    vertices = reverse $ foldl (\vs x -> if any (elem x) vs then vs else (orbit x (sigma m)):vs) [] (orbit (root m) (phi m))
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
           all (\m -> length (orbit (root m) (phi m)) == ndarts m) ms
  where
    ts = allNPT (2*n-1) n
    ms = nub $ map (canonifyCarte . nptToMap) ts

-- verified _test4 n for n <- [0..10]
