module LinLam.Diagrams.Matching where

import Data.Maybe
import Data.List

import Diagrams.Prelude
import Diagrams.Backend.SVG

matchingDiag :: Eq a => [(String,a)] -> [(String,a)] -> Diagram B -> Diagram B
matchingDiag upper lower d =
  flip (foldr $ \(s,t) d -> d # connectPerim' (with & arrowHead .~ noHead & arrowShaft .~ arc xDir (1/2 @@ turn)) s t (3/4 @@ turn) (3/4 @@ turn) # lwL 0.1) uu $
  flip (foldr $ \(s,t) d -> d # connectPerim' (with & arrowHead .~ noHead & arrowShaft .~ arc xDir (-1/2 @@ turn)) s t (1/4 @@ turn) (1/4 @@ turn) # lwL 0.1) ll $
  flip (foldr $ \(s,t) d -> d # connectPerim' (with & arrowHead .~ noHead) s t (3/4 @@ turn) (1/4 @@ turn) # lwL 0.1) ul $
  d
  where
    uu = [(s,t) | ((s,x),i) <- zip upper [1..], ((t,y),j) <- zip upper [1..], i < j, x == y]
    ll = [(s,t) | ((s,x),i) <- zip lower [1..], ((t,y),j) <- zip lower [1..], i < j, x == y]
    ul = [(s,t) | ((s,x),i) <- zip upper [1..], ((t,y),j) <- zip lower [1..], x == y]
