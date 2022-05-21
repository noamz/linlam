module LinLam.Utils where

import qualified Data.Set as Set

-- orbit of an element under a function
orbit :: Ord a => a -> (a -> a) -> [a]
orbit x f = go Set.empty x
  where
    go s x
      | Set.member x s = []
      | otherwise      = x : go (Set.insert x s) (f x)
