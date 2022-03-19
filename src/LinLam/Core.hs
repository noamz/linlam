module LinLam.Core where

import Data.List
import Data.Maybe
import qualified Data.MemoCombinators as Memo

import Control.Monad.State

import qualified LinLam.Permutations as P

-- datatype of linear lambda terms
data LT = V Int | A LT LT | L Int LT
  deriving (Show,Read)
-- note that the datatype does not exclude "pseudoterms", i.e., ill-scoped
-- terms like A (V 0) (L 0 (L 1 (V 1))) (representing "a(\a.\b.b)")

-- a term-in-context is a term together with a permutation of its free variables
type LTc = ([Int], LT)

-- check that a pseudo-term is well-scoped
wellScoped :: LT -> Bool
wellScoped t = go t (free t) == Just []
  where
    go (V x)   gamma
      | x `elem` gamma = Just (gamma \\ [x])
      | otherwise      = Nothing
    go (A t u) gamma = do
      gamma' <- go t gamma
      go u gamma'
    go (L x t) gamma = go t (x:gamma)

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

-- lambda closure of a term
closure :: LT -> LT
closure t = foldr L t (free t)

-- deconstruct a sequence of nested lambdas, returning the list of
-- lambda-bound variables in reverse and the body of the innermost lambda.
-- Satisfies foldl (flip L) u xs = t where (xs,u) = unlambdas t
unlambdas :: LT -> ([Int], LT)
unlambdas t = go t []
  where
    go (L x t) xs = go t (x:xs)
    go u       xs = (xs, u)

-- deconstruct a sequence of left-nested applications, returning the head
-- and the list of arguments in order.
-- Satisfies foldl A u us = t where (u,us) = unapps t
unapps :: LT -> (LT,[LT])
unapps t = go t []
  where
    go (A t u) us = go t (u:us)
    go u       us = (u, us)

-- support = set of all variables occurring in a term
support :: LT -> [Int]
support (V x)     = [x]
support (A t1 t2) = support t1 `union` support t2
support (L x t1)  = [x] `union` support t1

-- returns a variable that is fresh for a list of terms
fresh :: [LT] -> Int
fresh ts = 1 + maximum (-1 : concatMap support ts)

-- Generic routine for generating all terms with n subterms and k free variables.
-- Inputs:
--   pick : function taking a context and selects a variable to abstract
--   bridgeless : set to True to restrict to terms with no closed subterms
--   normal : set to True to restrict to terms with no beta redices
-- Output:
--   list of triples ([xs],n,t), where t is a term with free variables xs
--   and support of variables [0..n].
gentm :: ([Int] -> [(Int,[Int])]) -> Bool -> Bool -> Int -> Int -> [([Int],Int,LT)]
gentm pick bridgeless normal = Memo.memo2 Memo.integral Memo.integral gen'
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

-- naming scheme: L = linear, P = planar, B = bridgeless, N = normal, lr = "LR planarity"
allLT   = map thd3 .: gentm P.pick1 False False
allBLT  = map thd3 .: gentm P.pick1 True  False
allPT   = map thd3 .: gentm P.ucons False False
allBPT  = map thd3 .: gentm P.ucons True  False
allNLT  = map thd3 .: gentm P.pick1 False True
allNBLT = map thd3 .: gentm P.pick1 True  True
allNPT  = map thd3 .: gentm P.ucons False True
allNBPT = map thd3 .: gentm P.ucons True  True
allNPT_lr  = map thd3 .: gentm P.usnoc False True
allNBPT_lr = map thd3 .: gentm P.usnoc True  True

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

-- compares t1 and t2 lexicographically up to alpha equivalence, with an ordering where V < A < L
alphaLE :: LT -> LT -> Ordering
alphaLE (V x1)    (V x2)    = compare x1 x2
alphaLE (A t1 u1) (A t2 u2) = case alphaLE t1 t2 of
                                LT -> LT
                                GT -> GT
                                EQ -> alphaLE u1 u2
alphaLE (L x1 t1) (L x2 t2) = alphaLE t1 (swapname (x2,x1) t2)
alphaLE (V _)     _         = LT
alphaLE (A _ _)   (V _)     = GT
alphaLE (A _ _)   _         = LT
alphaLE (L _ _)   _         = GT

instance Eq LT where
  t1 == t2 = alphaEq t1 t2

instance Ord LT where
  compare t1 t2 = alphaLE t1 t2
  
-- use a canonical naming scheme for the variables in a term, with a
-- distinct name for each occurrence
canonify :: LT -> LT
canonify t = t'''
  where
    vs = free t
    n = fresh [t]
    t' = foldl (\u (x,y) -> swapname (x,y) u) t (zip vs [n..])
    t'' = fst $ runState (go t') (n + length vs)
    t''' = rename (\x -> x-n) t''
    go :: LT -> State Int LT
    go (V x)   = return (V x)
    go (L x t) = do
      n <- get
      put (n+1)
      t' <- go (swapname (x,n) t)
      return (L n t')
    go (A t u) = do
      t' <- go t
      u' <- go u
      return (A t' u')

-- rename the variables in t2 so that it has support disjoint from t1
shift :: LT -> LT -> LT
shift t1 t2 = rename (+fresh [t1]) t2

-- substitution
subst :: (LT,Int) -> LT -> LT
subst (u,x) t =
  case t of
    V y
      | y == x                -> u
      | otherwise             -> V y
    A t1 t2                   -> A (subst (u,x) t1) (subst (u,x) t2)
    L y t1
      | y == x                -> L y t1
      | not (y `elem` free u) -> L y (subst (u,x) t1)
      | otherwise             -> L z (subst (u,x) t1')
        where
          z = fresh [V x, t1, u]
          t1' = rename (\w -> if w == y then z else w) t1

-- faster version of substitution without occurs check, which can
-- be used when you know that the supports of u and t are distinct
subst' :: (LT,Int) -> LT -> LT
subst' (u,x) t =
  case t of
    V y
      | y == x                -> u
      | otherwise             -> V y
    A t1 t2                   -> A (subst' (u,x) t1) (subst' (u,x) t2)
    L y t1                    -> L y (subst' (u,x) t1)

-- applies a list of substitutions in sequential order
msubst :: [(LT,Int)] -> LT -> LT
msubst rho t = foldl (flip subst) t rho

-- datatype of one-hole contexts for linear terms
data LTdot = Hole | A'1 LTdot LT | A'2 LT LTdot | L' Int LTdot
  deriving (Show,Eq)

-- we represent contexts "inside-out", i.e., with the parts of the
-- context nearest to the hole at the top-level.
-- The plugging function is defined accordingly.
plug :: LTdot -> LT -> LT
plug Hole       u = u
plug (A'1 k t2) u = plug k (A u t2)
plug (A'2 t1 k) u = plug k (A t1 u)
plug (L' x k)   u = plug k (L x u)

-- sometimes we want to reverse a context "outside-in"
revcxt :: LTdot -> LTdot
revcxt k = revapp k Hole
  where
    revapp :: LTdot -> LTdot -> LTdot
    revapp Hole      k' = k'
    revapp (A'1 k t) k' = revapp k (A'1 k' t)
    revapp (A'2 t k) k' = revapp k (A'2 t k')
    revapp (L' x k)  k' = revapp k (L' x k')

-- a term-in-focus (a.k.a. "term zipper") is a pair of a context and a term
type LTfoc = (LTdot,LT)

-- focus on all possible subterms
focus :: LT -> [LTfoc]
focus t = go Hole t
  where
    go :: LTdot -> LT -> [LTfoc]
    go k t = (k,t) :
             case t of
               V x     -> []
               A t1 t2 -> go (A'1 k t2) t1 ++ go (A'2 t1 k) t2
               L x t1  -> go (L' x k) t1

-- list of subterms
subterms :: LT -> [LT]
subterms = map snd . focus

-- list of proper subterms
subterms' :: LT -> [LT]
subterms' = tail . subterms

-- focus on all possible beta-redices subterms
focusBeta :: LT -> [LTfoc]
focusBeta t = [(k,u) | (k,u@(A (L _ _) _)) <- focus t]

-- step of beta reduction
beta :: LT -> [LT]
beta t = do
  (k, A (L x t1) t2) <- focusBeta t
  return $ plug k (subst (t2,x) t1)

-- faster beta reduction, that can be used when you know that all
-- lambda bound variables are distinct
beta' :: LT -> [LT]
beta' t = do
  (k, A (L x t1) t2) <- focusBeta t
  return $ plug k (subst' (t2,x) t1)

-- test if beta normal
isNormal :: LT -> Bool
isNormal t = null (beta t)

-- test if "neutral" (= beta normal and not a lambda)
isNeutral :: LT -> Bool
isNeutral t = isNormal t && not (isLam t)

-- normalize a term
normalize :: LT -> LT
normalize t = until isNormal (head . beta) t

-- faster normalization, that can be used when you know that all
-- lambda bound variables are distinct
normalize' :: LT -> LT
normalize' t = until isNormal (head . beta') t

-- checks t1 =beta t2
betaEq :: LT -> LT -> Bool
betaEq t1 t2 = normalize t1 == normalize t2

-- checks t1 <=beta t2
betaLE :: LT -> LT -> Bool
betaLE t1 t2 = case compare (size t1) (size t2) of
  LT -> False
  EQ -> t1 == t2
  GT -> let n = size t2 in
        t2 `elem` (until (\ts -> null ts || size (head ts) == n) (foldr union [] . map beta) [t1])

-- computes levels of the beta reduction graph of a term
betaLevels :: LT -> [[LT]]
betaLevels t = [t] : go (beta t)
  where
    go :: [LT] -> [[LT]]
    go [] = []
    go us = us : go (foldr union [] (map beta us))

-- focus on all possible eta-redices subterms
focusEta :: LT -> [LTfoc]
focusEta t = [(k,u) | (k, u@(L x (A _ (V x')))) <- focus t, x == x']

-- step of eta reduction
eta :: LT -> [LT]
eta t = do
  (k, L _ (A t1 _)) <- focusEta t
  return $ plug k t1

-- eta-short normal form of a term
etaShort :: LT -> LT
etaShort t = until (null . eta) (head . eta) t

-- checks t1 =eta t2
etaEq :: LT -> LT -> Bool
etaEq t1 t2 = etaShort t1 == etaShort t2

-- checks t1 =betaeta t2
betaEtaEq :: LT -> LT -> Bool
betaEtaEq t1 t2 = etaShort (normalize t1) == etaShort (normalize t2)

-- fast test if a term is bridgeless
isBridgeless :: LT -> Bool
isBridgeless t = go t (\_ -> True)
  where
    go :: LT -> (Int -> Bool) -> Bool
    go (V x)   cont = cont 1
    go (A t u) cont = go t (\k1 -> go u (\k2 -> cont (k1+k2)))
    go (L x t) cont = go t (\k1 -> if k1 > 1 then cont (k1-1) else False)

-- focus on the (osensibly unique) occurrence(s) of a given free variable 
focusVar :: LT -> Int -> [LTdot]
focusVar t x = go Hole t
  where
    go k (V y)     = [k | x == y]
    go k (A t1 t2) = go (A'1 k t2) t1 ++ go (A'2 t1 k) t2
    go k (L y t1)  = if y /= x then go (L' y k) t1 else []
