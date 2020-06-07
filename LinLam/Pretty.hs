module LinLam.Pretty where

import Control.Monad.State
import Control.Monad.Identity

import LinLam

paren :: String -> String
paren s = "(" ++ s ++ ")"

varnames = map return ['a'..'z'] ++ ["X" ++ show n | n <- [0..]]

prettyLT_state :: [Int] -> LT -> StateT Int Identity String
prettyLT_state [x] V         = return $ varnames !! x
prettyLT_state xs  (A t1 t2) = do
  let xs2 = take (arity t2) xs
  let xs1 = drop (arity t2) xs
  s2 <- prettyLT_state xs2 t2
  s1 <- prettyLT_state xs1 t1
  return $ (if isVar t1 then id else paren) s1 ++ paren s2
prettyLT_state xs  (L i t1)  = do
  x <- get
  put (x+1)
  let (xs1,xs2) = splitAt i xs
  s1 <- prettyLT_state (xs1 ++ x : xs2) t1
  return $ "\\" ++ (varnames !! x) ++ "." ++ s1

prettyLTc  :: LTc -> String
prettyLTc (xs, t) = fst $ runState (prettyLT_state xs t) (length xs)

prettyLT  :: LT -> String
prettyLT t = prettyLTc (reverse [0..arity t-1], t)

printLT  = putStrLn . prettyLT
printLTc = putStrLn . prettyLTc
