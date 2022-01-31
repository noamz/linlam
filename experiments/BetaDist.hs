-- experiment measuring "distance to normal form" = the number of beta-reductions
-- needed to reach beta-normal form

import LinLam.Core
import LinLam.Random

main = mainExperiment (\t -> (size t - size (normalize t)) `div` 3)
