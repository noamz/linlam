-- Various routines for random generation of linear lambda terms.
-- Unless otherwise stated, "random" below always means that we generate
-- a uniformly random object of a given size.
module LinLam.Random where

import Data.List
import Data.Maybe
import qualified Data.Set as Set

import Control.Monad

import System.Random
import System.Environment
import qualified System.Random.Shuffle as RG

import LinLam.Core

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

-- return a list of random closed linear lambda terms of size n
randomLTs :: Int -> Int -> IO [LT]
randomLTs n p = mapM (\_ -> randomLT n) [1..p]

-- return a random 1-variable-open bridgeless linear lambda term of size n
randomBLT :: Int -> IO LT
randomBLT n = do
  t <- randomLT (n+1)
  case t of 
    L x u -> if isBridgeless u then return u else randomBLT n
    _     -> randomBLT n

-- sort date to produce a histogram
histogram :: Ord a => [a] -> [(a,Int)]
histogram xs = map (\ys -> (head ys, length ys)) $ group $ sort xs

-- given a statistic f of linear terms,
-- generate p closed terms of size n
-- and return the resulting histogram
experimentLT :: Ord a => (LT -> a) -> Int -> Int -> IO [(a,Int)]
experimentLT f n p = do
  let r = randomLT n
  ts <- sequence (replicate p r)
  return $ histogram (map f ts)

-- given a statistic f of bridgeless linear terms,
-- generate p 1-variable-open terms of size n
-- and return the resulting histogram
experimentBLT :: Ord a => (LT -> a) -> Int -> Int -> IO [(a,Int)]
experimentBLT f n p = do
  let r = randomBLT n
  ts <- sequence (replicate p r)
  return $ histogram (map f ts)

-- return the first and second moments of a distribution
moments :: Fractional a => [(a,Int)] -> (a,a)
moments xks = (mean,variance)
  where
    n = fromIntegral $ sum [k | (x,k) <- xks]
    mean = sum [x*fromIntegral(k) | (x,k) <- xks] / n
    variance = sum [(x-mean)*(x-mean)*fromIntegral(k) | (x,k) <- xks] / n

-- Wrapper building a top-level main function from an experiment.
-- The compiled program takes the size and number of trials as arguments,
-- and then outputs a histogram of the resulting distribution (and optionally
-- the mean and variance).
mainExperiment :: (LT -> Int) -> IO ()
mainExperiment exp = do
  name <- getProgName
  args <- getArgs
  if length args < 2 then
    do
      putStrLn ("Usage: " ++ name ++ " <size> <trials> [moments?]")
      fail "not enough arguments"
  else
    do
      let n = read (args !! 0)
      let p = read (args !! 1)
      let m = if length args > 2 then read (args !! 2) else False
      hist <- experimentLT exp (3*n+2) p
      putStrLn (show hist)
      if m then do
         let (mean,variance) = moments [(fromIntegral x,k) | (x,k) <- hist]
         putStrLn ("mean: " ++ show mean)
         putStrLn ("variance: " ++ show variance)
         return ()
      else return ()
