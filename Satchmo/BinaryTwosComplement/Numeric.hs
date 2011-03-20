module Satchmo.BinaryTwosComplement.Numeric where

import qualified Satchmo.BinaryTwosComplement.Op.Fixed as F
import qualified Satchmo.Numeric as N

instance N.Constant F.Number where
    constant = F.constant  
    
instance N.Create F.Number where    
    create = F.number

instance N.Numeric F.Number where
    equal = F.equals
    greater_equal = F.ge
    plus = F.add
    minus = F.subtract
    times = F.times 
