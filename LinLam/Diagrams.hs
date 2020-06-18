module LinLam.Diagrams where

import Data.Maybe
import Data.List
import Data.List.Split

import Diagrams.Prelude
-- import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG

import LinLam
import LinLam.Pretty

lamcolor = lightblue
appcolor = indianred

spot :: Diagram B
spot = circle 0.01 # lwL 0.1 # fc black & pad 120

attach :: Diagram B -> String -> String -> String -> Diagram B -> Diagram B
attach lab n1 n2 n =
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
  let p = head $ intersectPoints
          (position [(location b1,fromOffsets [ 100*unitX # rotateBy (7/8)])] :: Path V2 Double)
          (position [(location b2,fromOffsets [ 100*unitX # rotateBy (5/8)])] :: Path V2 Double)
  in
   atop $
   position [(p,lab # named n & pad 3)]

lamTree :: String -> LT -> (Diagram B, [String])
lamTree k (V _) = (spot # named k, [k])
lamTree k (A t1 t2) =
  let (d1,s1) = lamTree ('L' : k) t1 in
  let (d2,s2) = lamTree ('R' : k) t2 in
  let k1 = ('L' : k) in
  let k2 = ('R' : k) in
  (attach (circle 0.2 # lwL 0.1 # fc appcolor) k1 k2 k (d1 ||| d2)
   # (connectOutside' (with & arrowHead .~ noHead) k k1 # lwL 0.1)
   # (connectOutside' (with & arrowHead .~ noHead) k k2 # lwL 0.1),
   s1 ++ s2)
lamTree k (L _ t) =
  let (d1,s1) = lamTree ('L' : k) t in
  let (d2,s2) = (spot # named ('R' : k), ['R': k]) in
  let k1 = ('L' : k) in
  let k2 = ('R' : k) in
  (attach (circle 0.2 # lwL 0.1 # fc lamcolor) k1 k2 k (d1 ||| d2)
   # (connectOutside' (with & arrowHead .~ noHead) k k1 # lwL 0.1)
   # (connectOutside' (with & arrowHead .~ noHead) k k2 # lwL 0.1),
   s1 ++ s2)

data Arc = U Int | D Int
  deriving (Show,Eq,Ord)

diagArcs_glue :: [String] -> [Arc] -> Diagram B -> Diagram B
diagArcs_glue ns w d =
  let dict = zip w ns in
  let shaft = arc xDir (-1/2 @@ turn) in
  d #
  applyAll [connect' (with & arrowHead .~ noHead & arrowShaft .~ shaft) (fromJust $ lookup (U i) dict) (fromJust $ lookup (D i) dict) # lwL 0.1 | U i <- w, isJust (lookup (D i) dict)]

lt2arcs :: LT -> [Arc]
lt2arcs (V x)     = [U x]
lt2arcs (A t1 t2) = lt2arcs t1 ++ lt2arcs t2
lt2arcs (L x t1)  = lt2arcs t1 ++ [D x]

diagLT :: LT -> Diagram B
diagLT t =
  let (d,ns) = lamTree [] t in
  let w = lt2arcs t in
  let gd = diagArcs_glue ns w d in
  (withName ([] :: String) $ \b ->
   atop $ spot # named "root" # moveTo (location b) # translateY (-1))
  gd # (connectOutside' (with & arrowHead .~ noHead) "root" ([]::String) # lwL 0.1) 

renderLT :: LT -> String -> IO ()
renderLT t basename = do
  renderPretty (basename ++ ".svg") (mkWidth 1024) (mkDiagram t # pad 1.1)
    where
      mkDiagram t = vsep 1 [diagLT t # centerX, text (prettyLT t) # centerX]

renderLTs :: [LT] -> String -> IO ()
renderLTs ts basename = do
  renderPretty (basename ++ ".svg") (mkWidth 1024) (vsep 1.3 [hsep 1 ds # pad 1.1 | ds <- chunksOf r [mkDiagram t | t <- ts]])
    where
      r = ceiling (sqrt(fromIntegral $ length ts))
      mkDiagram t = vsep 1 [diagLT t # centerX, text (prettyLT t) # centerX]
