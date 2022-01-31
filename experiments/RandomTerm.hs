-- generate and print some random linear terms

import LinLam.Random
import LinLam.Pretty

import Control.Monad
import System.Environment
import System.Exit

main :: IO ()
main = do
  name <- getProgName
  args <- getArgs
  unless (length args >= 1) $ do
    putStrLn ("Usage: " ++ name ++ " <size> [<trials>]")
    exitFailure
  let n = read (args !! 0)
  let p = if length args >= 2 then read (args !! 1) else 1
  ys <- experimentLT id (3*n+2) p
  mapM_ printLT ys
