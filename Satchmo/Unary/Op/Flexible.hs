module Satchmo.Unary.Op.Flexible 
       
( module Satchmo.Unary.Op.Common 
, add
)       
       
where

import Prelude hiding ( not, and, or )
import qualified Prelude

import Satchmo.Boolean
import   Satchmo.Unary.Data
import Satchmo.Unary.Op.Common 

import Control.Monad ( forM )
import qualified Data.Map as M

-- | Unary addition. Output bit length is sum of input bit lengths.
add :: MonadSAT m => Number -> Number -> m Number
add a b = add_quadratic (width a + width b) a b


