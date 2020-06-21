module LinLam.BCI where

import Data.List
import Data.Maybe

import LinLam
import LinLam.Pretty

-- translation to BCI combinators as described in Chapter 6 of
-- "Combinatory Logic: volume 1" by Curry, Feys, Craig

data BCI = B | C | I | Var Int | App BCI BCI
  deriving (Show,Eq)

arity' :: BCI -> Int
arity' (Var _)   = 1
arity' (App t u) = arity' t + arity' u
arity' _         = 0

toLT :: BCI -> LT
toLT (Var x)   = V x
toLT (App t u) = A (toLT t) (toLT u)
toLT B         = L 0 $ L 1 $ L 2 $ A (V 0) (A (V 1) (V 2))
toLT C         = L 0 $ L 1 $ L 2 $ A (A (V 0) (V 2)) (V 1)
toLT I         = L 0 $ V 0

abstract :: Int -> BCI -> BCI
abstract i t = case t of
  Var _                    -> I
  App t1 t2 | i < arity' t1 -> foldl App C [abstract i t1, t2]
  App t1 t2 | otherwise    -> foldl App B [t1, abstract (i-arity' t1) t2]

fromLT :: LT -> BCI
fromLT (V x)   = Var x
fromLT (A t u) = App (fromLT t) (fromLT u)
fromLT (L x t) = abstract i (fromLT t)
  where
    Just i = elemIndex x (free t)

overheadBCI :: Fractional a => LT -> a
overheadBCI t = fromIntegral (size (toLT (fromLT t))) / fromIntegral (size t)
