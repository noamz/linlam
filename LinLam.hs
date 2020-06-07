module LinLam where

import Data.MemoCombinators as Memo

import qualified Permutations as P

-- datatype of linear lambda terms
data LT = V | A LT LT | L Int LT
  deriving (Show,Eq)
-- a term-in-context is a term together with a permutation of its free variables
type LTc = (P.Perm, LT)

-- classify terms by top-level constructor
isVar, isApp, isLam :: LT -> Bool
isVar V       = True
isVar _       = False
isApp (A _ _) = True
isApp _       = False
isLam (L _ _) = True
isLam _       = False

-- arity = number of free variables
arity :: LT -> Int
arity V         = 1
arity (A t1 t2) = arity t1 + arity t2
arity (L _ t1)  = arity t1 - 1

-- size = number of subterms
size :: LT -> Int
size V         = 1
size (A t1 t2) = 1 + size t1 + size t2
size (L _ t1)  = 1 + size t1

-- generate all terms-in-context with n subterms and k free variables
gentm :: (P.Perm -> P.Perm -> [P.Perm]) -> Bool -> Bool -> Int -> Int -> [LTc]
gentm shuffn bridgeless normal = Memo.memo2 integral integral gen'
  where
    gen = gentm shuffn bridgeless normal
    gen' n k
      | n <= 0 = []
      | n == 1 = [([0], V) | k == 1]
      | n > 1  = [(p, A t1 t2) |
                  n1 <- [1..n-1], k1 <- [0..k],
                  let k2 = k-k1, let n2 = n-1-n1,
                  (p1, t1) <- gen n1 k1,
                  not (normal && isLam t1),
                  (p2, t2) <- gen n2 k2,
                  p <- shuffn p2 p1]
                 ++
                 [(p, L i t1) |
                  not (bridgeless && k == 0),
                  (p1, t1) <- gen (n-1) (k+1),
                  let i = head p1,
                  let p = map (\j -> if j < i then j else j-1) (tail p1)]

allLT   = gentm P.shuffle False False
allBLT  = gentm P.shuffle True  False
allPT   = gentm P.uappend False False
allBPT  = gentm P.uappend True  False
allNLT  = gentm P.shuffle False True
allNBLT = gentm P.shuffle True  True
allNPT  = gentm P.uappend False True
allNBPT = gentm P.uappend True  True

-- substitution

subst :: (LT,Int) -> LT -> LT
subst (u,i) t = case t of
                  V
                    | i == 0 -> u
                    | otherwise -> V
                  A t1 t2
                    | i < arity t2 -> A t1 (subst (u,i) t2)
                    | otherwise    -> A (subst (u,i-arity t2) t1) t2
                  L j t1
                    | i < j     -> L (arity t - 1 + j) (subst (u,i) t1)
                    | otherwise -> L j (subst (u,i+1) t1)

-- datatype of one-hole contexts for linear terms
data LTdot = Hole | A'1 LTdot LT | A'2 LT LTdot | L' Int LTdot
  deriving (Show,Eq)

plug :: LTdot -> LT -> LT
plug Hole      t = t
plug (A'1 k u) t = A (plug k t) u
plug (A'2 u k) t = A u (plug k t)
plug (L' i k)  t = L i (plug k t)

-- focus on all possible subterms
focus :: LT -> [(LTdot,LT)]
focus t = (Hole,t) :
          case t of
            V       -> []
            A t1 t2 -> [(A'2 t1 k,u) | (k,u) <- focus t2] ++
                       [(A'1 k t2,u) | (k,u) <- focus t1]
            L i t1  -> [(L' i k,u) | (k,u) <- focus t1]

isBetaRedex :: LT -> Bool
isBetaRedex (A (L _ _) _) = True
isBetaRedex _             = False

-- focus on all possible beta-redices subterms
focusBeta :: LT -> [(LTdot,LT)]
focusBeta t = 
          case t of
            V       -> []
            A t1 t2 -> [(Hole,t) | isLam t1] ++
                       [(A'2 t1 k,u) | (k,u) <- focusBeta t2] ++
                       [(A'1 k t2,u) | (k,u) <- focusBeta t1]
            L i t1  -> [(L' i k,u) | (k,u) <- focusBeta t1]

beta :: LT -> [LT]
beta t = do
  (k, A (L i t1) t2) <- focusBeta t
  return $ plug k (subst (t2,i) t1)

isNormal :: LT -> Bool
isNormal t = null (beta t)

normalize :: LT -> LT
normalize t = until isNormal (head . beta) t

betaEq :: LT -> LT -> Bool
betaEq t1 t2 = normalize t1 == normalize t2
