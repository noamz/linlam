module LinLam where

import Data.List
import Data.MemoCombinators as Memo

import qualified Permutations as P

-- datatype of linear lambda terms
data LT = V Int | A LT LT | L Int LT
  deriving (Show,Eq)
-- a term-in-context is a term together with a permutation of its free variables
type LTc = ([Int], LT)

-- classify terms by top-level constructor
isVar, isApp, isLam :: LT -> Bool
isVar (V _)   = True
isVar _       = False
isApp (A _ _) = True
isApp _       = False
isLam (L _ _) = True
isLam _       = False

-- arity = number of free variables
arity :: LT -> Int
arity (V _)     = 1
arity (A t1 t2) = arity t1 + arity t2
arity (L _ t1)  = arity t1 - 1

-- size = number of subterms
size :: LT -> Int
size (V _)     = 1
size (A t1 t2) = 1 + size t1 + size t2
size (L _ t1)  = 1 + size t1

-- list of free vars in left-to-right order
free :: LT -> [Int]
free (V x)      = [x]
free (A t1 t2)  = free t1 ++ free t2
free (L x t1)   = free t1 \\ [x]

-- support = set of all variables occurring in a term
support :: LT -> [Int]
support (V x)     = [x]
support (A t1 t2) = support t1 `union` support t2
support (L x t1)  = [x] `union` support t1

-- Generic routine for generating all terms with n subterms and k free variables.
-- Inputs:
--   pick : function taking a context and selects a variable to abstract
--   bridgeless : set to True to restrict to terms with no closed subterms
--   normal : set to True to restrict to terms with no beta redices
-- Output:
--   list of triples ([xs],n,t), where t is a term with free variables xs
--   and support of variables [0..n].
gentm :: ([Int] -> [(Int,[Int])]) -> Bool -> Bool -> Int -> Int -> [([Int],Int,LT)]
gentm pick bridgeless normal = Memo.memo2 integral integral gen'
  where
    gen = gentm pick bridgeless normal
    gen' n k
      | n <= 0 = []
      | n == 1 = [([0], 0, V 0) | k == 1]
      | n > 1  = [(xs2' ++ xs1, 1+m1+m2, A t1 t2') |
                  n1 <- [1..n-1], k1 <- [0..k],
                  (xs1, m1, t1) <- gen n1 k1,
                  not (normal && isLam t1),
                  (xs2, m2, t2) <- gen (n-1-n1) (k-k1),
                  let t2' = rename (+(1+m1)) t2,
                  let xs2' = map (+(1+m1)) xs2]
                 ++
                 [(xs, m1, L x t1) |
                  not (bridgeless && k == 0),
                  (xs1, m1, t1) <- gen (n-1) (k+1),
                  (x,xs) <- pick xs1]

thd3 :: (a,b,c) -> c
thd3 (a,b,c) = c

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) f g a b = f (g a b)

-- naming scheme: L = linear, P = planar, B = bridgeless, N = normal
allLT   = map thd3 .: gentm P.pick1 False False
allBLT  = map thd3 .: gentm P.pick1 True  False
allPT   = map thd3 .: gentm P.ucons False False
allBPT  = map thd3 .: gentm P.ucons True  False
allNLT  = map thd3 .: gentm P.pick1 False True
allNBLT = map thd3 .: gentm P.pick1 True  True
allNPT  = map thd3 .: gentm P.ucons False True
allNBPT = map thd3 .: gentm P.ucons True  True

-- apply a variable renaming
rename :: (Int -> Int) -> LT -> LT
rename f (V x)     = V (f x)
rename f (A t1 t2) = A (rename f t1) (rename f t2)
rename f (L x t1)  = L (f x) (rename f t1)

-- apply a variable swapping
swapname :: (Int,Int) -> LT -> LT
swapname (x,y) = rename (\z -> if z == x then y else if z == y then x else z)

-- checks t1 =alpha t2
alphaEq :: LT -> LT -> Bool
alphaEq (V x1)    (V x2)    = x1 == x2
alphaEq (A t1 u1) (A t2 u2) = alphaEq t1 t2 && alphaEq u1 u2
alphaEq (L x1 t1) (L x2 t2) = alphaEq t1 (swapname (x2,x1) t2)
alphaEq _         _         = False

-- rename the variables in t2 so that it has support disjoint from t1
shift :: LT -> LT -> LT
shift t1 t2 = rename (+(1+maximum(support t1))) t2

-- substitution
subst :: (LT,Int) -> LT -> LT
subst (u,x) t =
  case t of
    V y
      | y == x                -> u
      | otherwise             -> V y
    A t1 t2
      | x `elem` free t1      -> A (subst (u,x) t1) t2
      | otherwise             -> A t1 (subst (u,x) t2)
    L y t1
      | y == x                -> L y t1
      | not (y `elem` free u) -> L y (subst (u,x) t1)
      | otherwise             -> L z (subst (u,x) t1')
        where
          z = 1 + maximum (x : support t1 ++ support u)
          t1' = rename (\w -> if w == y then z else w) t1

-- datatype of one-hole contexts for linear terms
data LTdot = Hole | A'1 LTdot LT | A'2 LT LTdot | L' Int LTdot
  deriving (Show,Eq)

plug :: LTdot -> LT -> LT
plug Hole       u = u
plug (A'1 k t2) u = A (plug k u) t2
plug (A'2 t1 k) u = A t1 (plug k u)
plug (L' x k)   u = L x (plug k u)

-- focus on all possible subterms
focus :: LT -> [(LTdot,LT)]
focus t = (Hole,t) :
          case t of
            V x     -> []
            A t1 t2 -> [(A'2 t1 k,u) | (k,u) <- focus t2] ++
                       [(A'1 k t2,u) | (k,u) <- focus t1]
            L x t1  -> [(L' x k,u)   | (k,u) <- focus t1]

-- focus on all possible beta-redices subterms
focusBeta :: LT -> [(LTdot,LT)]
focusBeta t = 
          case t of
            V x     -> []
            A t1 t2 -> [(Hole,t) | isLam t1] ++
                       [(A'2 t1 k,u) | (k,u) <- focusBeta t2] ++
                       [(A'1 k t2,u) | (k,u) <- focusBeta t1]
            L x t1  -> [(L' x k,u) | (k,u) <- focusBeta t1]

-- step of beta reduction
beta :: LT -> [LT]
beta t = do
  (k, A (L x t1) t2) <- focusBeta t
  return $ plug k (subst (t2,x) t1)

-- test if beta normal
isNormal :: LT -> Bool
isNormal t = null (beta t)

-- normalize a term
normalize :: LT -> LT
normalize t = until isNormal (head . beta) t

-- checks t1 =beta t2
betaEq :: LT -> LT -> Bool
betaEq t1 t2 = alphaEq (normalize t1) (normalize t2)

-- checks t1 <=beta t2
betaLE :: LT -> LT -> Bool
betaLE t1 t2 = alphaEq t1 t2 || any (\t1' -> betaLE t1' t2) (beta t1)

