module Satchmo.Unary.Op.Fixed 

( module Satchmo.Unary.Op.Common 
, add, add_quadratic
)       
       
where

import Prelude hiding ( not, and, or )
import qualified Prelude

import Satchmo.Boolean
import   Satchmo.Unary.Op.Common
import   Satchmo.Unary.Data

import Control.Monad ( forM, when )
import qualified Data.Map as M

add :: MonadSAT m => Number -> Number -> m Number
add = add_quadratic

-- | Unary addition. Output bit length is sum of input bit lengths.
add_quadratic :: MonadSAT m => Number -> Number -> m Number
add_quadratic a b = do
    let width = Prelude.max ( length $ bits a ) 
                            ( length $ bits b ) 
    t <- Satchmo.Boolean.constant True
    pairs <- forM ( zip [0..] $ t : bits a ) $ \ ( i,x) -> 
             forM ( zip [0..] $ t : bits b ) $ \ ( j,y) -> 
             do z <- and [x,y] ; return (i+j, [z])
    cs <- forM ( tail $ map snd $ M.toAscList $ M.fromListWith (++) $ concat pairs ) or
    let ( pre, post ) = splitAt width cs
    when ( Prelude.not $ null post ) $ assert [ not $ head post ]
    return $ make pre
    