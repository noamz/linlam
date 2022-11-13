-- Proof search to generate all normal (planar) terms of a given type
module LinLam.Search where

import LinLam.Core
import LinLam.Typing

-- data type of raw skeletons of sequent calculus proofs
data Pf = Ax | RImp Pf | LImp Pf Pf
  deriving (Show,Eq)

-- perform proof search using a focused sequent calculus, with
-- right-inversion and left-focus judgments
rinv :: [Type] -> Type -> [Pf]
rinv gamma (TFn a b) = RImp <$> rinv (a:gamma) b
rinv gamma (TVar p)
  | null gamma       = []
  | otherwise        = lfoc (init gamma) (last gamma) p

lfoc :: [Type] -> Type -> Int -> [Pf]
lfoc gamma (TVar q)  p = [Ax | null gamma && p == q]
lfoc gamma (TFn a b) p = do
  k <- [0..length gamma]
  let (gamma',delta) = splitAt k gamma
  u <- rinv delta a
  t <- lfoc gamma' b p
  return (LImp t u)

-- a proof can be converted to a linear lambda term
pfToLT :: Pf -> LT
pfToLT Ax       = V 0
pfToLT (RImp p) = L x t
  where
    t = pfToLT p
    x = last (free t)
pfToLT (LImp p1 p2) = subst (A (V x) t2,x) t1
  where
    t1 = pfToLT p1
    t2 = shift t1 (pfToLT p2)
    x = head (free t1)

-- generate all (planar) terms of a given type
allPTofType :: Type -> [LT]
allPTofType a = map pfToLT (rinv [] a)

-- Examples:
{-
*LinLam> printLTs $ allPTofType (readType "(b -> c) -> (a -> b) -> (a -> c)")
\a.\b.\c.a(b(c))
*LinLam> printLTs $ allPTofType (readType "(a -> a) -> (a -> a) -> (a -> a) -> (a -> a)")
\a.\b.\c.\d.a(b(c(d)))
*LinLam> printLTs $ allPTofType (readType "((a -> a) -> (a -> a) -> a) -> (a -> a) -> a")
\a.\c.a(\d.c(d))(\b.b)
\a.\b.a(\d.d)(\c.b(c))
-}
