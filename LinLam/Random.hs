module LinLam.Random where

import Data.List
import Data.Maybe
import qualified Data.Set as Set

import Control.Monad

import System.Random
import qualified System.Random.Shuffle as RG

import LinLam
import LinLam.Diagrams

data Carte = Carte { darts :: [Int], sigma :: [Int], alpha :: [Int] }
  deriving (Show,Eq)

root :: Carte -> Int
root m = head (darts m)

isConnected :: Carte -> Bool
isConnected m = go m [root m] Set.empty
  where
    go :: Carte -> [Int] -> Set.Set Int -> Bool
    go m []     s = all (\d -> Set.member d s) (darts m)
    go m (d:ds) s
      | Set.member d s = go m ds s
      | otherwise      = go m (alpha m !! d : sigma m !! d : ds) (Set.insert d s)

-- random rooted k-regular map, with n vertices of degree k and one degree-1 vertex
kregular :: Int -> Int -> IO Carte
kregular k n = do
  let darts = [0..k*n] 
  let alpha = [min (i-2*(i `mod` 2)+1) (k*n) | i <- darts]
  perm <- RG.shuffleM (tail darts)
  let kperm = [(0, 0)] ++ [ (perm !! i, perm !! (k*(i `div` k) + (i + 1) `mod` k)) | i <- map (\x -> x-1) (tail darts) ]
  let sigma = [ fromJust $ lookup i kperm | i <- darts ]
  return (Carte { darts = darts, sigma = sigma, alpha = alpha })

-- random connected k-regular map with n vertices
kregular' k n = do
  m <- kregular k n
  if isConnected m then return m else kregular' k n

-- convert a rooted 3-valent map to a linear lambda term
toLT :: Carte -> Int -> Set.Set Int -> Set.Set Int -> (LT, Set.Set Int)
toLT m r visited boundary
  | Set.member (alpha m !! r) boundary = (V r, Set.insert r visited)
  | otherwise =
    let r1 = sigma m !! (alpha m !! r) in
    let r2 = sigma m !! (sigma m !! (alpha m !! r)) in
    let (t1,vis') = toLT m r1 (Set.insert r visited) (Set.insert r2 boundary) in
    if Set.member (alpha m !! r2) vis' then
      (L (alpha m !! r2) t1, vis')
    else
      let (t2,vis'') = toLT m r2 vis' boundary in
      (A t1 t2, vis'')

-- return a random closed linear lambda term of size n
randomLT :: Int -> IO LT
randomLT n = do
  unless (n >= 2 && (n-2) `mod` 3 == 0) (fail "invalid size")
  let as = (n-2) `div` 3
  let ls = 1 + as
  m <- kregular' 3 (as + ls)
  let (t, _) = toLT m (root m) Set.empty Set.empty
  return (canonify t)

-- return a random 1-variable-open bridgeless linear lambda term of size n
randomBLT :: Int -> IO LT
randomBLT n = do
  t <- randomLT (n+1)
  case t of 
    L x u -> if isBridgeless u then return u else randomBLT n
    _     -> randomBLT n
