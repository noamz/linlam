-- experiment measuring the number of closed proper subterms of a
-- random linear term (= bridges in rooted trivalent maps).

-- For medium-sized terms and a large number of experiments, the result should
-- approximate a Poisson distribution, see https://arxiv.org/abs/2106.08291

import LinLam.Core
import LinLam.Random
 
main = mainExperiment (\t -> length [u | u <- subterms' t, arity u == 0])
