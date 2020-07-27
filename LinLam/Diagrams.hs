module LinLam.Diagrams where

import Data.Maybe
import Data.List

import Diagrams.Prelude
import Diagrams.Backend.SVG

import LinLam.Core as LL
import LinLam.Pretty as LLP
import LinLam.Typing as LLT

import LinLam.Diagrams.Tree
import LinLam.Diagrams.Matching
import LinLam.Diagrams.Grid

data VType = Use Int | Bnd Int
instance Eq VType where
  Use x == Bnd y = x == y
  Bnd x == Use y = x == y
  _     == _     = False

data NType = App | Lam

ltOpTree :: LT -> OpTree VType NType
ltOpTree (V x)     = Leaf (Use x)
ltOpTree (A t1 t2) = Node App [ltOpTree t1, ltOpTree t2]
ltOpTree (L x t1)  = Node Lam [ltOpTree t1, Leaf (Bnd x)]

ltSpec :: DiagSpec VType NType
ltSpec = Spec { ld = dld, nd = dnd, rd = drd }
  where
    dld :: VType -> Diagram B
    dld (Use x) = circle 0.04 # lwL 0.05 # lc black # fc black
    dld (Bnd x) = circle 0.04 # lwL 0.05 # lc black # fc black
    dnd :: NType -> Diagram B
    dnd App = circle 0.2 # lwL 0.1 # fc indianred
    dnd Lam = circle 0.2 # lwL 0.1 # fc lightblue
    drd :: Diagram B
    drd = phantom (circle 0.1 :: Diagram B)

diagLT :: LT -> Diagram B
diagLT t = matchingDiag [("f" ++ show x,Bnd x) | x <- xs] fringe (vsep (height d) [position [(p2 (location vx ^._x,0), dx x) | (k,Use x) <- fringe, let Just vx = lookupName k d], d])
  where
    t' = canonify t
    xs = free t'
    dx :: Int -> Diagram B
    dx x = phantom (circle 0.1 :: Diagram B) # named ("f" ++ show x)
    (d,fringe) = diagTree' ltSpec "" (ltOpTree t')

text' :: Double -> String -> Diagram B
text' s t = text t # fontSize (local s) <> strutY (s * 1.3) <> strutX (s * 0.5 * fromIntegral (length t))

diagLT' t = vsep 1 [diagLT t # centerX, text' 1 (LLP.prettyLT t) # centerX] # pad 1.5

renderLTs :: [LT] -> String -> IO ()
renderLTs ts basename = do
  renderPretty (basename ++ ".svg") (mkWidth 2048) (gridDiagrams [diagLT t | t <- ts] # centerXY # pad 1.1)

renderLTs' :: [LT] -> String -> IO ()
renderLTs' ts basename = do
  renderPretty (basename ++ ".svg") (mkWidth 2048) (gridDiagrams [diagLT' t | t <- ts] # centerXY # pad 1.1)

renderLT  t = renderLTs  [t]
renderLT' t = renderLTs' [t]

data XType = Pos Int | Neg Int
instance Eq XType where
  Pos x == Neg y = x == y
  Neg x == Pos y = x == y
  _     == _     = False

posOpTree, negOpTree :: Type -> OpTree XType NType
posOpTree (TVar x)    = Leaf (Pos x)
posOpTree (TFn t1 t2) = Node Lam [posOpTree t2, negOpTree t1]
negOpTree (TVar x)    = Leaf (Neg x)
negOpTree (TFn t1 t2) = Node App [posOpTree t1, negOpTree t2]

typeSpec :: DiagSpec XType NType
typeSpec = ltSpec { ld = dld }
  where
    dld :: XType -> Diagram B
    -- dld (Pos x) = (text' 0.1 (LLP.prettyTVar x) # fc black) `atop` (square 0.2 # lwL 0.05 # lc black # fc gold)
    dld (Pos x) = square 0.2 # lwL 0.05 # lc black # fc gold
    -- dld (Neg x) = (text' 0.1 (LLP.prettyTVar x) # fc white) `atop` (square 0.2 # lwL 0.05 # lc black # fc purple)
    dld (Neg x) = square 0.2 # lwL 0.05 # lc black # fc purple

tdiagNLT :: LT -> Diagram B
tdiagNLT t = matchingDiag (concatMap reverse upper) lower (vsep (max (height droot) (sum [width dx | dx <- dxs])) [hsep 1 [rotate (1/2 @@ turn) dx # alignT | dx <- dxs] # centerX, droot # centerX]) # centerY
  where
    (gamma, tau, xi) = synth t
    (droot,lower) = diagTree' typeSpec "" (posOpTree tau)
    (dxs,upper) = unzip (map (\(x,sigma) -> diagTree' (typeSpec { ld = rotate (1/2 @@ turn) . ld typeSpec }) (show x) (negOpTree sigma)) gamma)

tdiagNLT' t = vsep 1 [text' 1 (intercalate " , " (map (LLP.prettyType . snd) gamma)) # centerX, tdiagNLT t # centerX, text' 1 (LLP.prettyType tau) # centerX] # centerXY # pad 1.1
  where
    (gamma, tau, xi) = synth t

trenderNLTs :: [LT] -> String -> IO ()
trenderNLTs ts basename = do
  renderPretty (basename ++ ".svg") (mkWidth 2048) (gridDiagrams [tdiagNLT t | t <- ts] # centerXY # pad 1.1)

trenderNLTs' :: [LT] -> String -> IO ()
trenderNLTs' ts basename = do
  renderPretty (basename ++ ".svg") (mkWidth 2048) (gridDiagrams [tdiagNLT' t | t <- ts] # centerXY # pad 1.1)

trenderNLT  t = trenderNLTs  [t]
trenderNLT' t = trenderNLTs' [t]
