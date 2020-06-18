module LinLam.Pretty where

import Control.Monad.State
import Control.Monad.Identity

import LinLam

paren :: String -> String
paren s = "(" ++ s ++ ")"

varnames = map return ['a'..'z'] ++ ["X" ++ show n | n <- [0..]]

prettyLT  :: LT -> String
prettyLT (V x) = varnames !! x
prettyLT (A t1 t2)
  | isLam t1  = paren (prettyLT t1) ++ paren (prettyLT t2)
  | otherwise = prettyLT t1 ++ paren (prettyLT t2)
prettyLT (L x t1) = "\\" ++ (varnames !! x) ++ "." ++ prettyLT t1

prettyLTc = prettyLT . snd

printLT  = putStrLn . prettyLT
printLTc = putStrLn . prettyLTc
