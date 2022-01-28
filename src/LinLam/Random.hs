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
import System.Exit

import LinLam.Core
import LinLam.Maps

-- convert a rooted 3-valent map to a linear lambda term
toLT :: Carte -> Int -> Set.Set Int -> Set.Set Int -> (LT, Set.Set Int)
toLT m r visited boundary
  | Set.member (act (alpha m) r) boundary = (V r, Set.insert r visited)
  | otherwise =
    let r1 = act (sigma m) (act (alpha m) r) in
    let r2 = act (sigma m) (act (sigma m) (act (alpha m) r)) in
    let (t1,vis') = toLT m r1 (Set.insert r visited) (Set.insert r2 boundary) in
    if Set.member (act (alpha m) r2) vis' then
      (L (act (alpha m) r2) t1, vis')
    else
      let (t2,vis'') = toLT m r2 vis' boundary in
      (A t1 t2, vis'')

-- return a random closed linear lambda term of size n
randomLT :: Int -> IO LT
randomLT n = do
  unless (n >= 2 && (n-2) `mod` 3 == 0) (fail "invalid size")
  let as = (n-2) `div` 3
  let ls = 1 + as
  m <- randomKMap' 3 (as + ls)
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

-- given a statistic f of linear terms,
-- generate p closed terms of size n
-- and return the result of evaluating f
experimentLT :: (LT -> a) -> Int -> Int -> IO [a]
experimentLT f n p = do
  ts <- sequence (replicate p (randomLT n))
  return $ map f ts

-- given a statistic f of bridgeless linear terms,
-- generate p 1-variable-open terms of size n
-- and return the result of evaluating f
experimentBLT :: (LT -> a) -> Int -> Int -> IO [a]
experimentBLT f n p = do
  ts <- sequence (replicate p (randomBLT n))
  return $ map f ts

-- sort data to produce a histogram
histogram :: Ord a => [a] -> [(a,Int)]
histogram xs = map (\ys -> (head ys, length ys)) $ group $ sort xs

-- return the first and second moments of a distribution
moments :: Fractional a => [a] -> (a,a)
moments ys = (mean,variance)
  where
    n = fromIntegral $ length ys
    mean = sum ys / n
    variance = sum [(y-mean)*(y-mean) | y <- ys] / n

-- Wrapper building a top-level main function from an Int-valued experiment.
-- The compiled program takes the size and number of trials as arguments,
-- and then outputs a histogram of the resulting distribution (and optionally
-- the mean and variance).
mainExperiment :: (LT -> Int) -> IO ()
mainExperiment exp = do
  name <- getProgName
  args <- getArgs
  unless (length args >= 2) $ do
    putStrLn ("Usage: " ++ name ++ " <size> <trials> [moments?]")
    exitFailure
  let n = read (args !! 0)
  let p = read (args !! 1)
  let m = if length args > 2 then read (args !! 2) else False
  ys <- experimentLT exp (3*n+2) p
  putStrLn (show (histogram ys))
  when m $ do
    let (mean,variance) = moments (map fromIntegral ys)
    putStrLn ("mean: " ++ show mean)
    putStrLn ("variance: " ++ show variance)

-- Wrapper building a top-level main function from a float-valued experiment.
-- The compiled program takes the size and number of trials as arguments,
-- and then outputs the experimental data together with its mean and variance.
mainExperiment' :: (Show a,Fractional a) => (LT -> a) -> IO ()
mainExperiment' exp = do
  name <- getProgName
  args <- getArgs
  unless (length args >= 2) $ do
    putStrLn ("Usage: " ++ name ++ " <size> <trials> [moments?]")
    exitFailure
  let n = read (args !! 0)
  let p = read (args !! 1)
  let m = if length args > 2 then read (args !! 2) else False
  ys <- experimentLT exp (3*n+2) p
  putStrLn (show ys)
  when m $ do
    let (mean,variance) = moments ys
    putStrLn ("mean: " ++ show mean)
    putStrLn ("variance: " ++ show variance)
