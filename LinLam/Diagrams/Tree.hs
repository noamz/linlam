module LinLam.Diagrams.Tree where

import Data.List

import LinLam.Diagrams.Matching

import Diagrams.Prelude
import Diagrams.Backend.SVG

data OpTree l n = Leaf l | Node n [OpTree l n]

data DiagSpec l n = Spec { ld :: l -> Diagram B, nd :: n -> Diagram B, rd :: Diagram B }

attach :: Diagram B -> String -> String -> String -> Diagram B -> Diagram B
attach d n1 n2 n =
  withName n1 $ \b1 ->
  withName n2 $ \b2 ->
  let p = head $ intersectPoints
          (position [(location b1,fromOffsets [ 500*unitX # rotateBy (7/8)])] :: Path V2 Double)
          (position [(location b2,fromOffsets [ 500*unitX # rotateBy (5/8)])] :: Path V2 Double) ++ [p2 (0,0)]
  in
   atop (position [(p,d # named n)])

diagTree :: DiagSpec l n -> String -> OpTree l n -> (Diagram B, [(String,l)])
diagTree spec k (Leaf l)    = (ld spec l # named k, [(k,l)])
diagTree spec k (Node n []) = (nd spec n # named k, [])
diagTree spec k (Node n [t]) =
  ((withName ("1." ++ k) $ \b ->
      atop (nd spec n # named k # moveTo (location b) # translateY (-1)))
   d # (connectOutside' (with & arrowHead .~ noHead) k ("1." ++ k) # lwL 0.1),
   fringe)
  where
    (d,fringe) = diagTree spec ("1." ++ k) t
diagTree spec k (Node n ts) =
  (foldr (\i d -> lwL 0.1 (connectOutside' (with & arrowHead .~ noHead) k (show i ++ "." ++ k) d)) (attach (nd spec n) k1 kn k (hsep 1 (map fst subds))) [1..length ts],
   concatMap snd subds)
  where
    k1 = "1." ++ k
    kn = show (length ts) ++ "." ++ k
    subds = [diagTree spec (show i ++ "." ++ k) t | (i,t) <- zip [1..] ts]
    (d1,_) = head subds
    (dn,_) = last subds

diagTree' :: DiagSpec l n -> String -> OpTree l n -> (Diagram B, [(String,l)])
diagTree' spec k t =
  ((withName k
  (\b -> atop (rd spec # named "root" # moveTo (location b) # translateY (-1))))
   d # (connectOutside' (with & arrowHead .~ noHead) k "root" # lwL 0.1),
   fringe)
  where
    (d,fringe) = diagTree spec k t
  
diagGluedTrees :: Ord l => DiagSpec l n -> OpTree l n -> OpTree l n -> Diagram B
diagGluedTrees spec t1 t2 =
  matchingDiag fringe2 fringe1 (vsep (max (height d1) (height d2)) [reflectY d2, d1])
  where
    (d1,fringe1) = diagTree' spec "t1" t1
    (d2,fringe2) = diagTree' (spec { ld = reflectY . (ld spec) }) "t2" t2

data Ntype = Y | Z

t1, t2, t3, t4 :: OpTree Int Ntype
t1 = Node Z [Node Y [Leaf 1, Node Z [Leaf 2, Leaf 3], Leaf 4], Leaf 5, Node Y [Leaf 6, Leaf 7]]
t2 = Node Z [Node Y [Leaf 2, Leaf 1], Leaf 4, Node Y [Leaf 5, Node Z [Leaf 6, Leaf 7], Leaf 3]]
t3 = Node Z [Node Y [Leaf 2, Leaf 1], Leaf 4, Node Y [Leaf 5, Node Z [Leaf 6, Leaf 7], Leaf 3, Leaf 8, Leaf 8]]
t4 = Node Z [Leaf 10, Node Y [Leaf 1, Node Z [Leaf 9, Leaf 2, Leaf 3], Leaf 4], Leaf 5, Node Y [Leaf 6, Leaf 7, Leaf 9, Leaf 10]]

test :: OpTree Int Ntype -> OpTree Int Ntype -> String -> IO ()
test t1 t2 basename = 
  renderPretty (basename ++ ".svg") (mkWidth 1024) (diagGluedTrees spec t1 t2 # centerXY # pad 1.1)
  where
    spec :: DiagSpec Int Ntype
    spec = Spec { ld = dld, nd = dnd, rd = drd }
    dld :: Int -> Diagram B
    dld n = (text (show n) # fc white # scale 0.2 # centerXY) `atop` (square 0.4 # lwL 0.1 # fc green)
    dnd :: Ntype -> Diagram B
    dnd Y = circle 0.2 # lwL 0.1 # fc indianred
    dnd Z = circle 0.2 # lwL 0.1 # fc lightblue
    drd :: Diagram B
    drd = square 0.2 # lwL 0.1 # fc brown


