-- | Simple state transition system:
-- a state is a list of Booleans,
-- a transition is the flip of exactly one bit.
--
-- Calling the main program with "./State w::Int d::Int"
-- produces the qbf for 'there is a path of length <= 2^d
-- from state False^w to state True^w'.
-- The formula size is linear in w*d.
-- Test cases: "./State 4 2" (satisfiable), "./State 5 2" (unsatisfiable).
--
-- Output is an element near the middle of the path.
-- You may check that about half the bits are flipped 
-- (if the depth bound is tight).

import Satchmo.Boolean
import Satchmo.Code
import Satchmo.Counting
import Satchmo.Solver.Quantor 
import Satchmo.Solver.Qube 

import Prelude hiding ( not, and, or )

import Data.List ( tails )
import Data.Set ( Set )
import qualified Data.Set as S
import Control.Monad ( guard, forM )
import Data.Ix ( range )
import System.Environment

main = do
    argv <- getArgs
    let [ w, d ] = map read argv
    result <- Satchmo.Solver.Qube.solve $ form w d
    print result

form :: Int -> Int -> SAT ( Decoder [ Bool ] )
form width depth = do
    start <- forM [ 1 .. width ] $ \ n -> constant False
    goal  <- forM [ 1 .. width ] $ \ n -> constant True
    ( p, mid ) <- path depth start goal
    assert [ p ]
    return $ decode mid

-- | is there a path of length <= 2^depth ? 
-- If so, the second component is a state from the middle of the path
path :: Int -> [ Boolean ] -> [ Boolean ] -> SAT ( Boolean, [ Boolean ] )
path depth from to =
    if depth > 0 
    then do
        mid <- forM [ 1 .. length from ] $ \ _ -> exists
        p <- forM [ 1 .. length from ] $ \ _ -> forall
        q <- forM [ 1 .. length from ] $ \ _ -> forall
        pre <- monadic or [ monadic and [ equals from p , equals mid q ]
                          , monadic and [ equals mid  p , equals to  q ]
                          ]
        ( post, _ ) <- path (depth - 1) p q
        ok <- or [ not pre, post ]
        return ( ok, mid )
    else do
        ok <- monadic or [ onestep from to, equals from to ]
        return ( ok, from )

equals :: [ Boolean ] -> [ Boolean ] -> SAT Boolean
equals xs ys = monadic and 
             $ for ( zip xs ys ) $ \ (x,y) -> fmap not $ xor [x,y]

onestep :: [ Boolean ] -> [ Boolean ] -> SAT Boolean
onestep xs ys = do
    changes <- forM ( zip xs ys ) $ \ (x,y) -> xor [x,y]
    exactly 1 changes

for = flip map
