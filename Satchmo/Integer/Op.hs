-- | all operations have fixed bit length,
-- and are unsatisfiable in case of overflows.

module Satchmo.Integer.Op 

( negate, add, sub, times
, gt, ge, eq 
)

where

import Satchmo.Integer.Data
import Prelude hiding ( and, or, not, negate )
import Satchmo.Boolean hiding ( constant )
import qualified  Satchmo.Boolean as B

import qualified Satchmo.Binary.Op.Common as C
import qualified Satchmo.Binary.Op.Flexible as F

import Control.Monad ( forM, when )

-- | negate. Unsatisfiable if value is lowest negatve.
negate :: MonadSAT m 
       => Number -> m Number
negate n = do
    let ys = map B.not $ bits n 
    o <- B.constant True
    ( zs, c ) <- increment ys o
    assertOr [ last $ ys, B.not $ last zs ]
    return $ make zs

increment [] z = return ( [], z )
increment (y:ys) z = do
    ( r, d ) <- C.half_adder y z
    ( rs, c ) <- increment ys d
    return ( r : rs, c )

add :: MonadSAT m 
    => Number -> Number 
    -> m Number
add a b = do
    when ( width a /= width b ) 
    	 $ error "Satchmo.Integer.Op.add"
    cin <- B.constant False
    ( zs, cout ) <- 
        F.add_with_carry cin ( bits a ) ( bits b )
    monadic assertOr [ fun2 (==) cout $ last zs ]
    return $ make zs

sub :: MonadSAT m 
    => Number -> Number 
    -> m Number
sub a b = do
    when ( width a /= width b ) 
    	 $ error "Satchmo.Integer.Op.sub"
    c <- negate b
    add a c

times :: MonadSAT m 
    => Number -> Number 
    -> m Number
times a b = do
    when ( width a /= width b ) 
    	 $ error "Satchmo.Integer.Op.times"
    c <- F.times ( F.make $ bits a ) 
      	 	 ( F.make $ bits b )
    let ( pre, post ) = splitAt ( width a ) $ F.bits c
    monadic assertOr [ fun2 (==) ( head post) $ last pre ]
    return $ make pre

----------------------------------------------------

positive :: MonadSAT m
	 => Number 
	 -> m Boolean
positive n = do
    ok <- or $ init $ bits n   
    and [ ok, not $ last $ bits n ]

negative :: MonadSAT m
	 => Number 
	 -> m Boolean
negative n = do
    return $ last $ bits n

nonnegative :: MonadSAT m
	 => Number 
	 -> m Boolean
nonnegative n = do
    return $ not $ last $ bits n

----------------------------------------------------

eq :: MonadSAT m 
   => Number -> Number
   -> m Boolean
eq a b = do
    when ( width a /= width b ) 
    	 $ error "Satchmo.Integer.Op.eq"
    eqs <- forM ( zip ( bits a ) ( bits b ) )
    	   $ \ (x,y) -> fun2 (==) x y
    and eqs

gt :: MonadSAT m 
   => Number -> Number
   -> m Boolean
gt a b = do
    diff <- and [ not $ last $ bits a, last $ bits b ]
    same <- fun2 (==) ( last $ bits a )	
     	     	       ( last $ bits b )
    g <- F.gt ( F.make $ bits a ) 
      	      ( F.make $ bits b )
    monadic or [ return diff
    	       , and [ same, g ]
	       ]

ge :: MonadSAT m 
   => Number -> Number
   -> m Boolean
ge a b = do
    diff <- and [ not $ last $ bits a, last $ bits b ]
    same <- fun2 (==) ( last $ bits a )	
     	     	       ( last $ bits b )
    g <- F.ge ( F.make $ bits a ) 
      	      ( F.make $ bits b )
    monadic or [ return diff
    	       , and [ same, g ]
	       ]
    
