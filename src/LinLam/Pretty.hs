module LinLam.Pretty where

import Data.Char

import LinLam.Core
import LinLam.Typing

paren :: String -> String
paren s = "(" ++ s ++ ")"

varnames = map return ['a'..'z'] ++ ["X" ++ show n | n <- [0..]]

prettyLT  :: LT -> String
prettyLT (V x) = varnames !! x
prettyLT (A t1 t2)
  | isLam t1  = paren (prettyLT t1) ++ paren (prettyLT t2)
  | otherwise = prettyLT t1 ++ paren (prettyLT t2)
prettyLT (L x t1) = "\\" ++ (varnames !! x) ++ "." ++ prettyLT t1

prettyLTc :: LTc -> String
prettyLTc = prettyLT . snd

printLT  = putStrLn . prettyLT
printLTc = putStrLn . prettyLTc

printLTs :: [LT] -> IO ()
printLTs  = mapM_ printLT
printLTcs :: [LTc] -> IO ()
printLTcs = mapM_ printLTc

prettyTVar :: Int -> String
prettyTVar i
  | i < 25    = [chr (945 + i)]
  | otherwise = "X" ++ show (i - 25)

prettyType :: Type -> String
prettyType (TVar x)  = prettyTVar x
prettyType (TFn a b) = paren (prettyType a ++ " -> " ++ prettyType b)

printType = putStrLn . prettyType
