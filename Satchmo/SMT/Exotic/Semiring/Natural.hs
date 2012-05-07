module Satchmo.SMT.Exotic.Semiring.Natural where

import Satchmo.SMT.Exotic.Semiring.Class

instance Semiring Integer where    
  strictness _ = Full
  nonnegative x = x >= 0 ; strictly_positive x = x >= 1 
  ge = (>=) ; gt = (>)
  plus = (+) ; zero = 0 ; times = (*) ; one = 1
