module Satchmo.BinaryTwosComplement.Op.Common
    (equals, eq, lt, le, ge, gt, positive, negative, nonNegative)
where

import Prelude hiding (and,or,not)
import Satchmo.MonadSAT (MonadSAT)
import Satchmo.BinaryTwosComplement.Data (Number,toUnsigned,msb,bits)
import Satchmo.Boolean (Boolean,and,or,not,ifThenElseM)
import qualified Satchmo.Boolean as Boolean
import qualified Satchmo.Binary.Op.Common as B

sameSign, negativePositive :: MonadSAT m => Number -> Number -> m Boolean
sameSign a b = Boolean.equals [msb a, msb b]
negativePositive a b = and [msb a, not $ msb b]

equals,eq,lt,le,ge,gt :: MonadSAT m => Number -> Number -> m Boolean
equals a b = B.equals (toUnsigned a) (toUnsigned b)
eq = equals

lt a b = ifThenElseM ( sameSign a b )
                     ( B.lt (toUnsigned a) (toUnsigned b) )
                     ( negativePositive a b )

le a b = ifThenElseM ( sameSign a b )
                     ( B.le (toUnsigned a) (toUnsigned b) )
                     ( negativePositive a b )

ge = flip le
gt = flip lt

positive,negative,nonNegative :: MonadSAT m => Number -> m Boolean
positive a = do
  one <- or $ bits a
  and [not $ msb a, one]

negative = return . msb

nonNegative = return . not . msb