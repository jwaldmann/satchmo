import qualified Satchmo.Integer as I
import qualified Satchmo.Boolean as B
import Satchmo.SAT.Mini ( solve )

import Test.SmallCheck
import Test.SmallCheck.Series
import Control.Monad ( guard )

tdecode (Positive w) x = monadic $ do
  out <- Satchmo.SAT.Mini.solve $ do
      a <- I.constant w x
      return $ I.decode w a
  print (x,out)    
  return $ Just x == out

representable w x =
  let h = 2^(w-1) in negate h <= x && x < h

tfun2 e f (Positive w) x y = monadic $
  if representable w x && representable w y
  then do
    out <- Satchmo.SAT.Mini.solve $ do
      a <- I.constant w x
      b <- I.constant w y
      c <- e a b
      return $ I.decode w c
    print (x,y,out)
    let expect = do
          let z = f x y
          guard $ representable w z
          return z
    return $ expect == out
  else return True
  
tadd = tfun2 I.add (+)
tmult = tfun2 I.times (*)

