-- implements the bijection from "A correspondence between rooted planar maps and normal planar lambda terms" (https://lmcs.episciences.org/1598)

module LinLam.Tutte where

import LinLam.Core
import LinLam.Cartes
import LinLam.Pretty

import Data.List

import qualified Math.Combinat.Permutations as P

-- inductive data type of Tutte decompositions of rooted planar maps
data Tutte = Trivial                    -- trivial map
           | RootBridge Tutte Tutte     -- map with a bridge root formed from a pair of maps
           | RootNonBridge Int Tutte    -- map with a non-bridge root formed by drawing an edge along the perimeter of a map
  deriving (Show,Eq)

-- focus on a "neutral handle" of a normal LR-planar term (see LMCS paper for explanation of terminology)
focusNeutralHandle :: LT -> [(LTdot,LT)]
focusNeutralHandle t = [(Hole,t) | isNeutral t] ++
                       case t of
                         V x     -> []
                         A t1 t2 -> [(A'2 t1 k,u) | (k,u) <- focusNeutralHandle t2] ++
                                    [(A'1 k t2,u) | arity t2 == 0, (k,u) <- focusNeutralHandle t1]
                         L x t1  -> [(L' x k,u)   | (k,u) <- focusNeutralHandle t1]

-- count the index of a context representing a neutral handle
handleIndex :: LTdot -> Int
handleIndex Hole      = 0
handleIndex (L' _ k)  = handleIndex k
handleIndex (A'2 _ k) = 1 + handleIndex k
handleIndex (A'1 k t) = 1 + length (focusNeutralHandle t) + handleIndex k

-- tutte decomposition of an LR-planar normal term
nptlrTutte :: LT -> Tutte
nptlrTutte (L x t) = case revcxt (head $ focusVar t x) of
  A'2 u k -> RootNonBridge (handleIndex k) (nptlrTutte (plug (revcxt k) u))
  A'1 k u -> RootBridge (nptlrTutte (L x (plug (revcxt k) (V x)))) (nptlrTutte u)
  _       -> Trivial
nptlrTutte u = error ("nptlrTutte: non-normal term " ++ prettyLT u)

-- reverse transformation, interpreting tutte decompositions as LR-planar normal terms
tutteNPTLR :: Tutte -> LT
tutteNPTLR Trivial            = L 0 $ V 0
tutteNPTLR (RootBridge m1 m2) = L x $ subst (A (V x) t2,x) t1
  where
    L x t1 = tutteNPTLR m1
    t2     = tutteNPTLR m2
tutteNPTLR (RootNonBridge i m1) = L x $ plug k (A u (V x))
  where
    t1 = tutteNPTLR m1
    (k,u) = focusNeutralHandle t1 !! i
    x = fresh [t1]

-- test that the composition is the identity
_test1 :: Int -> Bool
_test1 n = all (\t -> tutteNPTLR (nptlrTutte t) == t) (allNPT_lr (3*n+2) 0)

-- verified _test1 n for n <- [0..6]

-- interpret a tutte decomposition as a rooted planar map
tutteCarte :: Tutte -> Carte
tutteCarte Trivial            = trivialMap
tutteCarte (RootBridge m1 m2) = Carte { ndarts = ndarts', sigma = sigma', alpha = alpha' }
  where
    c1 = tutteCarte m1
    c2 = tutteCarte m2
    ndarts' = ndarts c1 + ndarts c2 + 1
    sigma1' = P.toPermutation $ 2 : map (\x -> if x == 1 then 1 else x+1) (P.fromPermutation (sigma c1))
    sigma'  = sigma1' `P.concatPermutations` sigma c2
    alpha'  = (P.transposition ndarts' (2,2+ndarts c1)) `P.multiplyPermutation`
              (P.toPermutation [1] `P.concatPermutations` alpha c1 `P.concatPermutations` alpha c2)
              
tutteCarte (RootNonBridge i m1) = Carte { ndarts = ndarts', sigma = sigma', alpha = alpha' }
  where
    c1 = tutteCarte m1
    ndarts' = ndarts c1 + 2
    rface = orbit (P.inversePermutation (sigma c1) P.!!! (root c1)) (phi c1)
    perimeter = length rface
    dart = rface !! i
    alpha' = (P.transposition ndarts' (2,ndarts')) `P.multiplyPermutation`
             (P.toPermutation [1] `P.concatPermutations` alpha c1 `P.concatPermutations` P.toPermutation [1])
    sigma' = P.toPermutation $ 2 : 
      map (\x -> if (i > 0 && x == sigma c1 P.!!! dart) || (i == 0 && x == root c1) then ndarts' else if x == 1 then 1 else x+1)
      (P.fromPermutation (sigma c1)) ++ [if i == 0 then 1 else 1 + sigma c1 P.!!! dart]

-- convert a LR-planar normal term to a rooted planar map
nptlrCarte :: LT -> Carte
nptlrCarte = tutteCarte . nptlrTutte

-- test that the image of nptlrCarte is the set of rooted planar maps with n edges
_test2 :: Int -> Bool
_test2 n = length cs == length ts &&
          all ((==1) . euler) cs
  where
    ts = allNPT_lr (3*n+2) 0
    cs = nub $ map (canonifyCarte . nptlrCarte) ts

-- verified _test2 n for n <- [0..6]
