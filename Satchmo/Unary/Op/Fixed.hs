module Satchmo.Unary.Op.Fixed 

( module Satchmo.Unary.Op.Common 
, add
)       
       
where

import Prelude hiding ( not, and, or )
import qualified Prelude

import Satchmo.Boolean
import   Satchmo.Unary.Op.Common
import   Satchmo.Unary.Data

import Control.Monad ( forM, when, guard )
import qualified Data.Map as M

add :: MonadSAT m => Number -> Number -> m Number
add a b = 
    -- add_quadratic 
    add_via_merge
        (Just $ Prelude.max ( width a ) ( width b )) a b


    