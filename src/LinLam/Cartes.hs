module LinLam.Cartes where

import Data.List
import Data.Maybe
import qualified Data.Set as Set

import System.Random

import qualified Data.MemoCombinators as Memo
import qualified Math.Combinat.Permutations as P

import LinLam.Utils

-- some routines on permutations
type Perm = P.Permutation
act = (P.!!!)

-- remove a given list of indices from a permutation
deletePerm :: Perm -> [Int] -> Perm
deletePerm pi ds =
  P.toPermutation [rename (fromJust $ find (\j -> not (elem j ds)) (tail (orbit i (act pi)) ++ [i])) | i <- ixs]
  where
    ixs = [1..P.permutationSize pi] \\ ds
    rename j = fromJust (lookup j $ zip ixs [1..])

-- type of combinatorial maps
data Carte = Carte { ndarts :: Int, sigma :: Perm, alpha :: Perm }
  deriving (Show,Eq,Ord)

-- compute face permutation of a map
phi :: Carte -> Perm
phi m = P.inversePermutation (P.multiplyPermutation (sigma m) (alpha m))

-- we always consider 1 as the root dart
root :: Carte -> Int
root m = 1

-- we can reroot at another dart by applying an appropriate transposition
reroot :: Int -> Carte -> Carte
reroot x m = m { sigma = conjugate swap (sigma m),
                 alpha = conjugate swap (alpha m) }
  where
    swap = P.transposition (ndarts m) (1,x)

-- the trivial/terminal rooted map with a single dart
trivialMap :: Carte
trivialMap = Carte { ndarts = 1, sigma = P.toPermutation [1], alpha = P.toPermutation [1] }

-- the two one-edge rooted maps
bridgeMap, loopMap :: Carte
bridgeMap = Carte { ndarts = 3, sigma = P.toPermutation [3,2,1], alpha = P.toPermutation [1,3,2] }
loopMap   = Carte { ndarts = 3, sigma = P.toPermutation [3,1,2], alpha = P.toPermutation [1,3,2] }

-- the dual of a map, exchanging vertices with faces
dualMap :: Carte -> Carte
dualMap m = m { sigma = P.inversePermutation (phi m) }

-- test if a map is connected
isConnected :: Carte -> Bool
isConnected m = go Set.empty [root m]
  where
    go :: Set.Set Int -> [Int] -> Bool
    go s []     = Set.size s == ndarts m
    go s (d:ds)
      | Set.member d s = go s ds
      | otherwise      = go (Set.insert d s) (alpha m P.!!! d : sigma m P.!!! d : ds)

-- perform a canonical dfs traversal of a map, starting from a given
-- list of darts, excluding some set of darts
dfsCarte' :: Carte -> [Int] -> Set.Set Int -> [Int]
dfsCarte' m xs s = go s xs []
  where
    go :: Set.Set Int -> [Int] -> [Int] -> [Int]
    go s []     visited = reverse visited
    go s (d:ds) visited
      | Set.member d s = go s ds visited
      | otherwise      = go (Set.insert d s) (alpha m P.!!! d : sigma m P.!!! d : ds) (d:visited)

-- perform a canonical dfs traversal of a connected rooted map
dfsCarte :: Carte -> [Int]
dfsCarte m = dfsCarte' m [root m] Set.empty

-- relabel a map canonically by its dfs traversal
canonifyCarte :: Carte -> Carte
canonifyCarte m = 
  Carte { ndarts = ndarts m, sigma = conjugate pi (sigma m), alpha = conjugate pi (alpha m) }
  where
    pi = P.inversePermutation $ P.toPermutation (dfsCarte m)

-- generate a random not-necessarily-connected rooted map with n edges + 1 root half-edge
randomMap :: Int -> IO Carte
randomMap n = do
  let ndarts = 1+2*n
  let alpha = 1 : [min (i-2*(i `mod` 2)+1) (2*n+1) | i <- [2..ndarts]]
  sigma <- getStdRandom $ P.randomPermutation ndarts
  return (Carte { ndarts = ndarts, sigma = sigma, alpha = P.toPermutation alpha })

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
  perm <- getStdRandom $ P.randomPermutation (ndarts-1)
  let sigma_cycles = P.DisjointCycles ([1] : chunksOf k (map (1+) $ P.fromPermutation perm))
  return (Carte { ndarts = ndarts, sigma = P.disjointCyclesToPermutation ndarts sigma_cycles, alpha = P.toPermutation alpha })
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
  sigma <- P.permutations ndarts
  return (Carte { ndarts = ndarts, sigma = sigma, alpha = P.toPermutation alpha })

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
permCycles pi = fixedPoints pi ++ P.fromDisjointCycles (P.permutationToDisjointCycles pi)
  where
    fixedPoints pi = [[i] | i <- [1..P.permutationSize pi], pi P.!!! i == i]

-- computes "passport" of a map = degrees of the vertices, edges, faces
passport :: Carte -> ([Int],[Int],[Int])
passport m = (sort $ map length $ permCycles (sigma m),
              sort $ map length $ permCycles (alpha m),
              sort $ map length $ permCycles (phi m))

-- simple statistics of maps
numVerts, numEdges, numFaces :: Carte -> Int
numVerts = length . permCycles . sigma
numEdges = length . permCycles . alpha
numFaces = length . permCycles . phi

-- Euler characteristic of a map
euler :: Carte -> Int
euler m = numVerts m - numEdges m + numFaces m

-- conjugate the second permutation by the first
conjugate :: Perm -> Perm -> Perm
conjugate f pi = P.productOfPermutations [f, pi, P.inversePermutation f]

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
  let n = P.permutationSize alpha in
  map (P.toPermutation . map snd . sort) $ match_cycles calpha cbeta

-- generate all n cyclic permutations of a list of length n
cycleonce :: [a] -> [[a]]
cycleonce xs = cycle' xs []
  where
    cycle' :: [a] -> [a] -> [[a]]
    cycle' [] acc = []
    cycle' (x:xs) acc = [x:xs ++ reverse acc] ++ cycle' xs (x:acc)

-- remove a list of darts from a map while simultaneously rerooting it
dartsDelete :: Carte -> [Int] -> Int -> Carte
dartsDelete m ds r =
  reroot r' $
  Carte { ndarts = ndarts m - length ds,
          sigma = deletePerm (sigma m) ds,
          alpha = deletePerm (alpha m) ds }
  where
    r' = fromJust (lookup r $ zip ([1..ndarts m] \\ ds) [1..])
