-- | attempt factorization of integer.
-- | run like this: ./test/Factor 1000000000001
-- (takes 10 .. 20 seconds depending on your CPU)

{-# language PatternSignatures #-}

import Prelude hiding ( not )

import Satchmo.Binary.Op.Fixed 
import qualified Satchmo.Binary.Op.Flexible 
import Satchmo.SAT.Mini
import Satchmo.Boolean hiding ( equals )
import Satchmo.Code

import System.Environment

main :: IO ()
main = do
    [ n ] <- getArgs
    res :: Maybe [ Integer ] <- solve $ do
        x <- Satchmo.Binary.Op.Flexible.constant $ read n
        a <- number $ width x 
        notone a
        b <- number $ width x  
        notone b
        ab <- times a b
        monadic assert [ equals ab x ]
        return $ decode [ a, b ]
    print res

notone f = do
    one <- Satchmo.Binary.Op.Flexible.constant 1
    e <- equals f one
    assert [ not e ]
