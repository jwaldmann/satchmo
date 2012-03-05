module Satchmo.Unary.Op.Flexible 
       
( module Satchmo.Unary.Op.Common 
, plus
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
plus :: MonadSAT m => Number -> Number -> m Number
plus a b = do
    t <- Satchmo.Boolean.constant True
    pairs <- forM ( zip [0..] $ t : bits a ) $ \ ( i,x) -> 
             forM ( zip [0..] $ t : bits b ) $ \ ( j,y) -> 
             do z <- and [x,y] ; return (i+j, [z])
    cs <- forM ( map snd $ tail $ M.toAscList $ M.fromListWith (++) $ concat pairs ) or
    return $ make cs

    