module LinLam.Maps where

import Data.List
import Data.Maybe
import qualified Data.Set as Set

import System.Random

import qualified Data.MemoCombinators as Memo
import qualified Math.Combinat.Permutations as CP

type Perm = CP.Permutation
act = (CP.!!!)

-- type of combinatorial maps
data Carte = Carte { ndarts :: Int, sigma :: Perm, alpha :: Perm }
  deriving (Show,Eq,Ord)

-- compute face permutation of a map
phi :: Carte -> Perm
phi m = CP.inversePermutation (CP.multiplyPermutation (sigma m) (alpha m))

-- we always consider 1 as the root dart
root :: Carte -> Int
root m = 1

-- test if a map is connected
isConnected :: Carte -> Bool
isConnected m = go Set.empty [root m]
  where
    go :: Set.Set Int -> [Int] -> Bool
    go s []     = Set.size s == ndarts m
    go s (d:ds)
      | Set.member d s = go s ds
      | otherwise      = go (Set.insert d s) (alpha m CP.!!! d : sigma m CP.!!! d : ds)

-- perform a canonical dfs traversal of a connected map
dfsCarte :: Carte -> [Int]
dfsCarte m = go Set.empty [root m] []
  where
    go :: Set.Set Int -> [Int] -> [Int] -> [Int]
    go s []     visited = reverse visited
    go s (d:ds) visited
      | Set.member d s = go s ds visited
      | otherwise      = go (Set.insert d s) (alpha m CP.!!! d : sigma m CP.!!! d : ds) (d:visited)

-- relabel a map canonically by its dfs traversal
canonifyCarte :: Carte -> Carte
canonifyCarte m = 
  Carte { ndarts = ndarts m, sigma = conjugate pi (sigma m), alpha = conjugate pi (alpha m) }
  where
    pi = CP.inversePermutation $ CP.toPermutation (dfsCarte m)

-- generate a random not-necessarily-connected rooted map with n edges + 1 root half-edge
randomMap :: Int -> IO Carte
randomMap n = do
  let ndarts = 1+2*n
  let alpha = 1 : [min (i-2*(i `mod` 2)+1) (2*n+1) | i <- [2..ndarts]]
  sigma <- getStdRandom $ CP.randomPermutation ndarts
  return (Carte { ndarts = ndarts, sigma = sigma, alpha = CP.toPermutation alpha })

-- generate a random connected rooted map with n edges + 1 root half-edge
randomMap' :: Int -> IO Carte
randomMap' n = do
  m <- randomMap n
  if isConnected m then return m else randomMap' n

-- generate a random rooted k-regular map, with n vertices of degree k and one degree-1 vertex
randomKMap :: Int -> Int -> IO Carte
randomKMap k n = do
  let ndarts = 1+k*n
  let alpha = [min (i+2*(i `mod` 2)-1) ndarts | i <- [1..ndarts]]
  perm <- getStdRandom $ CP.randomPermutation (ndarts-1)
  let sigma_cycles = CP.DisjointCycles ([1] : chunksOf k (map (1+) $ CP.fromPermutation perm))
  return (Carte { ndarts = ndarts, sigma = CP.disjointCyclesToPermutation ndarts sigma_cycles, alpha = CP.toPermutation alpha })
  where
    chunksOf k [] = []
    chunksOf k xs = take k xs : chunksOf k (drop k xs)

-- generate a random connected k-regular map with n vertices
randomKMap' k n = do
  m <- randomKMap k n
  if isConnected m then return m else randomKMap' k n

-- generate all not-necessarily-connected rooted maps with n edges + 1 root half-edge
rootedMaps :: Int -> [Carte]
rootedMaps n = do
  let ndarts = 1+2*n
  let alpha = 1 : [min (i-2*(i `mod` 2)+1) (2*n+1) | i <- [2..ndarts]]
  sigma <- CP.permutations ndarts
  return (Carte { ndarts = ndarts, sigma = sigma, alpha = CP.toPermutation alpha })

-- generate a connected rooted map with n edges + 1 root half-edge
-- with one canonical representative per equivalence class
rootedMaps' :: Int -> [Carte]
rootedMaps' = Memo.integral gen
  where
    gen = Set.toList . Set.fromList . map canonifyCarte . filter isConnected . rootedMaps

-- list the cycles of a permutation:
-- the function "permutationToDisjointCycles" from Math.Combinat.Permutations
-- does this except that it omits fixed points.
permCycles :: Perm -> [[Int]]
permCycles pi = fixedPoints pi ++ CP.fromDisjointCycles (CP.permutationToDisjointCycles pi)
  where
    fixedPoints pi = [[i] | i <- [1..CP.permutationSize pi], pi CP.!!! i == i]

-- computes "passport" of a map = degrees of the vertices, edges, faces
passport :: Carte -> ([Int],[Int],[Int])
passport m = (sort $ map length $ permCycles (sigma m),
              sort $ map length $ permCycles (alpha m),
              sort $ map length $ permCycles (phi m))

-- Euler characteristic of a map
euler :: Carte -> Int
euler m = nverts - nedges + nfaces
  where
    nverts = length $ permCycles (sigma m)
    nedges = length $ permCycles (alpha m)
    nfaces = length $ permCycles (phi m)

-- conjugate the second permutation by the first
conjugate :: Perm -> Perm -> Perm
conjugate f pi = CP.productOfPermutations [f, pi, CP.inversePermutation f]

-- test whether two maps are equivalent up to relabelling
carteEquiv :: Carte -> Carte -> Bool
carteEquiv m1 m2 =
  -- first check that they have the same size
  ndarts m1 == ndarts m2 &&
  -- ...and the same passports ...
  passport m1 == passport m2 &&
  -- then try to find a unifier
  any (\p -> alpha m2 == conjugate p (alpha m1)) (residual (phi m1) (phi m2))

-- generate all n ways of removing an element from a list of length n
remove :: [a] -> [(a,[a])]
remove [] = []
remove (a:as) = (a,as) : [(a',a:as') | (a',as') <- remove as]

-- try to match the cycles of a permutation
match_cycles :: [(Int,[Int])] -> [(Int,[Int])] -> [[(Int,Int)]]
match_cycles [] [] = return []
match_cycles ((n,c):cs) mds = do
      let (d's, mds') = partition (\ (m,d) -> m == n) mds
      (d, d''s) <- remove (map snd d's)
      p <- match_cycles cs ((map (\d -> (n,d)) d''s) ++ mds')
      q <- match_cycle c d
      return (p ++ q)
match_cycle :: [Int] -> [Int] -> [[(Int,Int)]]
match_cycle c d = do
      d' <- cycleonce d
      return (zip c d')

-- given two permutations alpha and beta, "residual alpha beta"
-- computes a list of permutations p such that
--   conjugate p alpha `eqperm` beta
residual :: Perm -> Perm -> [Perm]
residual alpha beta =
  -- compute cyclic decompositions of alpha and beta
  let calpha = map (\c -> (length c, c)) (permCycles alpha) in
  let cbeta = map (\c -> (length c, c)) (permCycles beta) in
  let n = CP.permutationSize alpha in
  map (CP.toPermutation . map snd . sort) $ match_cycles calpha cbeta

-- generate all n cyclic permutations of a list of length n
cycleonce :: [a] -> [[a]]
cycleonce xs = cycle' xs []
  where
    cycle' :: [a] -> [a] -> [[a]]
    cycle' [] acc = []
    cycle' (x:xs) acc = [x:xs ++ reverse acc] ++ cycle' xs (x:acc)

-- orbit of an element under a permutation
orbit :: Int -> Perm -> [Int]
orbit x pi = go Set.empty x
  where
    go s x
      | Set.member x s = []
      | otherwise      = x : go (Set.insert x s) (pi CP.!!! x)
