module LinLam.Diagrams.Grid where

import Data.List
import Data.List.Split

import Diagrams.Prelude
import Diagrams.Backend.SVG

gridDiagrams :: [Diagram B] -> Diagram B
gridDiagrams ds = vcat [hcat [(d # centerXY) `atop` cell | d <- row] | row <- chunksOf r ds]
  where
    r = ceiling (sqrt(fromIntegral $ length ds))
    maxWidth  = maximum [width d  | d <- ds]
    maxHeight = maximum [height d | d <- ds]
    cell = square (1.2 * max maxWidth maxHeight) # lwL 0.1
