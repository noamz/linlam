module LinLam.Typing where

import Control.Monad.State

import Data.List
import Data.Maybe

import LinLam.Core

data Type = TVar Int | TFn Type Type
  deriving (Show,Eq,Ord)

type TCtx = [(Int,Type)]

isTVar (TVar _)  = True
isTVar (TFn _ _) = False

isTFn (TVar _)   = False
isTFn (TFn _ _)  = True

-- compute the order of a type
orderType :: Type -> Int
orderType (TVar _)  = 0
orderType (TFn a b) = max (1 + orderType a) (orderType b)

-- substitute a type for a type variable in a type
substT :: (Int,Type) -> Type -> Type
substT (x,t0) (TVar y) = if x == y then t0 else (TVar y)
substT (x,t0) (TFn t1 t2) = TFn (substT (x,t0) t1) (substT (x,t0) t2)

-- apply a type variable renaming
renameT :: (Int -> Int) -> Type -> Type
renameT f (TVar x)  = TVar (f x)
renameT f (TFn t1 t2) = TFn (renameT f t1) (renameT f t2)

-- apply a type variable swapping
swapnameT :: (Int,Int) -> Type -> Type
swapnameT (x,y) = renameT (\z -> if z == x then y else if z == y then x else z)

-- checks equivalence of types up to renaming of variables
alphaTEq' :: Type -> Type -> [(Int,Int)] -> Maybe [(Int,Int)]
alphaTEq' (TVar x1)   (TVar x2)   rho = 
  case find (\(y,z) -> y == x1) rho of
    Just (y,z) -> if z == x2 then Just rho else Nothing
    Nothing -> Just ((x1,x2) : rho)
alphaTEq' (TFn t1 u1) (TFn t2 u2) rho = do
  rho'  <- alphaTEq' t1 t2 rho
  rho'' <- alphaTEq' u1 u2 rho'
  return rho''
alphaTEq' _           _           rho = Nothing

alphaTEq t1 t2 = isJust (alphaTEq' t1 t2 [])

-- bidirectional type inference

data Straints = Straints { tvars :: Int, straints :: [(Type,Type)] }
  deriving (Show,Eq)

check :: Monad m => LT -> Type -> StateT Straints m TCtx
infer :: Monad m => LT -> StateT Straints m (TCtx,Type)

check (V x)   tau = return [(x,tau)]
check (A t u) tau = do
  (delta, sigma) <- infer u
  gamma <- check t (TFn sigma tau)
  return (gamma ++ delta)
check t       tau = do
  (gamma, sigma) <- infer t
  xi <- get
  put (xi { straints = (sigma,tau) : straints xi })
  return gamma

infer (L x t) = do
  (gamma, tau) <- infer t
  let (gammax,gamma0) = partition ((==x) . fst) gamma
  case gammax of
    [(_,sigma)] -> return (gamma0, TFn sigma tau)
    _           -> error ("non-linear use of " ++ show x)
infer t       = do
  xi <- get
  let alpha = tvars xi
  put (xi { tvars = 1 + tvars xi })
  gamma <- check t (TVar alpha)
  return (gamma, (TVar alpha))

synth :: LT -> (TCtx,Type,[(Type,Type)])
synth t = (gamma, tau, straints xi)
  where
    ((gamma, tau), xi) = runState (infer t) (Straints { tvars = 0, straints = [] })

synthClosed :: LT -> Type
synthClosed t = if arity t > 0 then error "synthClosed: term not closed" else tau
  where
    (_, tau, _) = synth t

choose :: (a -> Bool) -> [a] -> [(a,[a])]
choose p []     = []
choose p (a:as) = [(a,as) | p a] ++ [(b,a:bs) | (b,bs) <- choose p as]

match :: [(Type,Type)] -> [[(Type,Type)]]
match xi = do
  ((TFn a1 a2, TFn b1 b2), xi') <- choose (\(a,b) -> isTFn a && isTFn b) xi
  return ((b1,a1) : (a2,b2) : xi')

trans :: [(Type,Type)] -> [[(Type,Type)]]
trans xi = do
  ((a, TVar x), xi')  <- choose (\(a,b) -> isTVar b) xi
  ((_, b),      xi'') <- choose (\(a,b) -> a == TVar x) xi'
  return ((a,b) : xi'')

transatm :: [(Type,Type)] -> [[(Type,Type)]]
transatm xi = do
  ((a, TVar x), xi')  <- choose (\(a,b) -> isTVar b) xi
  ((_, b),      xi'') <- choose (\(a,b) -> a == TVar x) xi'
  if isTVar a || isTVar b then return ((a,b) : xi'') else []

nf :: [a -> [a]] -> a -> a
nf fs x =
  case concatMap (uncurry ($)) (zip fs (repeat x)) of
    (x':_) -> nf fs x'
    []     -> x

tnormalize :: (TCtx, Type, [(Type,Type)]) -> (TCtx, Type)
tnormalize (gamma, tau, xi) = (map (\(x,sigma) -> (x, phi sigma)) gamma, phi tau)
  where
    xi'  = nf [match,trans] xi
    neg  = filter (\(a,b) -> isTVar a) xi'
    pos  = filter (\(a,b) -> isTVar b) (xi' \\ neg)
    phiN sigma = foldr (\(TVar x,b) sigma -> substT (x,b) sigma) sigma neg
    phiP sigma = foldr (\(a,TVar x) sigma -> substT (x,a) sigma) sigma pos
    phi sigma = iterate (phiN . phiP) sigma !! 20

tnormalize' :: (TCtx, Type, [(Type,Type)]) -> (TCtx, Type)
tnormalize' (gamma, tau, xi) = (map (\(x,sigma) -> (x, phi sigma)) gamma, phi tau)
  where
    xi'  = nf [match,transatm] xi
    neg  = filter (\(a,b) -> isTVar a) xi'
    pos  = filter (\(a,b) -> isTVar b) (xi' \\ neg)
    phiN sigma = foldr (\(TVar x,b) sigma -> substT (x,b) sigma) sigma neg
    phiP sigma = foldr (\(a,TVar x) sigma -> substT (x,a) sigma) sigma pos
    phi sigma = iterate (phiN . phiP) sigma !! 20

testtt :: LT -> Bool
testtt t = alphaTEq a a'
  where
    (gamma,  a)  = tnormalize (synth t)
    (gamma', a') = tnormalize (synth (normalize t))

testtt' :: LT -> Bool
testtt' t = alphaTEq a a'
  where
    (gamma,  a)  = tnormalize' (synth t)
    (gamma', a') = tnormalize' (synth (normalize t))
    
  
