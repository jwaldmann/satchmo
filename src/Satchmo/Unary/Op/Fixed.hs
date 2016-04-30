module Satchmo.Unary.Op.Fixed 

( module Satchmo.Unary.Op.Common 
, add
, add_quadratic
, add_by_odd_even_merge
, add_by_bitonic_sort
)       
       
where

import Prelude hiding ( not, and, or )
import qualified Prelude

import Satchmo.Boolean
import   Satchmo.Unary.Data
import qualified Satchmo.Unary.Op.Common as C
import Satchmo.Unary.Op.Common hiding
  (add_quadratic, add_by_odd_even_merge, add_by_bitonic_sort)

import Control.Monad ( forM, when, guard )
import qualified Data.Map as M

add :: MonadSAT m => Number -> Number -> m Number
add = add_quadratic

add_quadratic a b = 
    C.add_quadratic (Just $ Prelude.max ( width a ) ( width b )) a b

add_by_odd_even_merge a b = 
    C.add_by_odd_even_merge (Just $ Prelude.max ( width a ) ( width b )) a b

add_by_bitonic_sort a b = 
    C.add_by_bitonic_sort (Just $ Prelude.max ( width a ) ( width b )) a b


    