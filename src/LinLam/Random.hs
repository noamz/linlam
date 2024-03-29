-- Various routines for random generation of linear lambda terms.
-- Unless otherwise stated, "random" below always means that we generate
-- a uniformly random object of a given size.
module LinLam.Random where

import Data.List
import Data.Maybe

import Control.Monad

import System.Random
import System.Environment
import System.Exit

import LinLam.Core
import LinLam.Cartes
import LinLam.Trivalent (toLT)

-- return a random closed linear lambda term of size n
randomLT :: Int -> IO LT
randomLT n = do
  unless (n >= 2 && (n-2) `mod` 3 == 0) (fail "invalid size")
  let as = (n-2) `div` 3
  let ls = 1 + as
  m <- randomKMap' 3 (as + ls)
  let t = toLT m
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

-- Generic wrapper building a top-level main function from an experiment.
-- The compiled program takes the size and number of trials as arguments,
-- and then outputs a histogram of the resulting distribution.
mainExperiment_generic :: (Show a,Ord a) => (LT -> a) -> IO ()
mainExperiment_generic exp = do
  name <- getProgName
  args <- getArgs
  unless (length args == 2) $ do
    putStrLn ("Usage: " ++ name ++ " <size> <trials>")
    exitFailure
  let n = read (args !! 0)
  let p = read (args !! 1)
  ys <- experimentLT exp (3*n+2) p
  putStrLn (show (histogram ys))

-- Wrapper building a top-level main function from an Int-valued experiment.
-- The compiled program takes the size and number of trials as arguments,
-- and then outputs a histogram of the resulting distribution (and optionally
-- the mean and variance).
mainExperiment_int :: (LT -> Int) -> IO ()
mainExperiment_int exp = do
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

mainExperiment = mainExperiment_int

-- Wrapper building a top-level main function from a float-valued experiment.
-- The compiled program takes the size and number of trials as arguments,
-- and then outputs the experimental data together with its mean and variance.
mainExperiment_float :: (Show a,Fractional a) => (LT -> a) -> IO ()
mainExperiment_float exp = do
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

mainExperiment' = mainExperiment_float
