-- some routines for manipulating operadic trees

module LinLam.OpTree where

import Data.List
import Data.Maybe

-- an operadic tree can have both leaves and nodes.
data OpTree l n = Leaf l | Node n [OpTree l n]
-- the nodes are interpreted as atomic operations which can be plugged
-- together to form composite operations, while the leaves are interpreted
-- as the global inputs to the tree seen as a composite operation.

-- returns the list of leaves of an operadic tree, i.e., its inputs
fringe :: OpTree l n -> [l]
fringe (Leaf l) = [l]

-- substitute a tree for a leaf of a tree
subst :: Eq l => (OpTree l n,l) -> OpTree l n -> OpTree l n
subst (u,x) t =
  case t of
    Leaf y
      | y == x    -> u
      | otherwise -> Leaf y
    Node c ts     -> Node c (map (subst (u,x)) ts)

-- unification

type Subst l n = [(OpTree l n, l)]

idsub :: [l] -> Subst l n
idsub xs = [(Leaf x,x) | x <- xs]

matchRoot :: (Eq l,Eq n) => OpTree l n -> OpTree l n -> Maybe (Subst l n, Subst l n)
matchRoot (Leaf x)    u              = return ([(u,x)], idsub (fringe u))
matchRoot t           (Leaf y)       = return (idsub (fringe t), [(t,y)])
matchRoot (Node c ts) (Node d us)
  | c == d && length ts == length us = matchRoots ts us
  | otherwise                        = Nothing

matchRoots :: (Eq l,Eq n) => [OpTree l n] -> [OpTree l n] -> Maybe (Subst l n, Subst l n)
matchRoots []     []     = return ([],[])
matchRoots (t:ts) (u:us) = do
  (s1,s2) <- matchRoot t u
  (w1,w2) <- matchRoots ts us
  return (s1++w1, s2++w2)
