module Permutations where

type Perm = [Int]

-- shuffle product of two permutations
shuffle :: Perm -> Perm -> [Perm]
shuffle xs ys = shuff (length xs) xs ys
  where
    shuff :: Int -> Perm -> Perm -> [Perm]
    shuff n []     ys     = [map (n+) ys]
    shuff n xs     []     = [xs]
    shuff n (x:xs) (y:ys) = [x : w | w <- shuff n xs (y:ys)] ++
                            [n+y : w | w <- shuff n (x:xs) ys]

-- deterministic concatenation of two permutations
uappend :: Perm -> Perm -> [Perm]
uappend = (\xs ys -> [xs ++ map (length xs+) ys])

pick1 :: [a] -> [(a,[a])]
pick1 []     = []
pick1 (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- pick1 xs]

ucons :: [a] -> [(a,[a])]
ucons [] = []
ucons (x:xs) = [(x,xs)]

-- operadic substitution of permutations
subst :: (Perm,Int) -> Perm -> Perm
subst (xs,i) ys = map shift ys0 ++ map (+i) xs ++ map shift ys1
  where
    (ys0,_:ys1) = span (/=i) ys
    k = length xs
    shift j = if j < i then j else j+k-1
