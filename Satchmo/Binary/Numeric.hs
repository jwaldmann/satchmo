module Satchmo.Binary.Numeric where

import qualified Satchmo.Binary.Op.Flexible as F
import qualified Satchmo.Numeric as N

instance N.Constant F.Number where
    constant = F.constant  
    
instance N.Create F.Number where    
    create = F.number

instance N.Numeric F.Number where
    equal = F.equals
    greater_equal = F.ge
    plus = F.add
    minus = error "Satchmo.Binary does not implement minus"
    times = F.times 
