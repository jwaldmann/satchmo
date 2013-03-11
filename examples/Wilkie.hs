-- | this was suggested by Levent Erkok
-- https://github.com/LeventErkok/sbv/issues/61

{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

import qualified Satchmo.Boolean as B
import qualified Satchmo.Code as C
import qualified Satchmo.Counting as C
import qualified Satchmo.SAT.Mini
import qualified Data.Map as M
import Control.Monad ( forM, forM_, void, when )

run :: Int -> IO ()
run d = do
    Just out <- Satchmo.SAT.Mini.solve $ do
        funs @ [ plus, times, exp ] <- 
            sequence $ replicate 3 $ mkfun d 2
        let value k e = inter plus times exp d k e
        forM_ tarski $ \ (k, (l,r)) -> do
            ll <- value k l ; rr <- value k r
            ok <- equals ll rr
            B.assert [ ok ]
        let (k,(l,r)) = wilkie
        ll <- value k l ; rr <- value k r
        ok <- equals ll rr
        B.assert [ B.not ok ]        
        return ( C.decode funs 
              :: Satchmo.SAT.Mini.SAT [Fun Bool] 
               )
    print out
    

tarski =
    let x = Proj 1 ; y = Proj 2 ; z = Proj 3 in
    [ (2, (Apply Plus x y
          , Apply Plus  y x) )
    , (3, ( Apply Plus (Apply Plus x y) z 
          , Apply Plus x (Apply Plus  y z)))

    , (2, (Apply Times x y
          , Apply Times  y x) )
    , (3, ( Apply Times (Apply Times x y) z 
          , Apply Times x (Apply Times  y z)))

    , (1, (Apply Times x One, x ))

    , (1, (Apply Exp x One, x ))
    , (1, (Apply Exp One x, One ))

    , (3, (Apply Times x (Apply Plus y z)
          , Apply Plus (Apply Times x y)(Apply Times x z)))

    , (3, (Apply Exp x (Apply Plus y z)
          , Apply Times (Apply Exp x y)(Apply Exp x z)))
    , (3, (Apply Exp (Apply Times x y) z
          , Apply Times (Apply Exp x z)(Apply Exp y z)))   

    , (3, (Apply Exp (Apply Exp x y) z
          , Apply Exp x (Apply Times y z)))
    ] 


-- | http://en.wikipedia.org/wiki/Tarski%27s_high_school_algebra_problem#Solution
-- \left((1+x)^y+(1+x+x^2)^y\right)^x
-- \cdot\left((1+x^3)^x+(1+x^2+x^4)^x\right)^y
-- \\&\quad=
-- \left((1+x)^x+(1+x+x^2)^x\right)^y
-- \cdot\left((1+x^3)^y+(1+x^2+x^4)^y\right)^x.

wilkie  = 
    let x = Proj 1 ; y = Proj 2
        x2 = Apply Times x x
        x3 = Apply Times x2 x
        x4 = Apply Times x2 x2
        a = Apply Plus One x
        b = foldr1 (Apply Plus) [ One, x, x2 ]
        c = Apply Plus One x3
        d = foldr1 (Apply Plus) [One, x2, x4 ]
        form p q i o = Apply Exp 
          (Apply Plus (Apply Exp p i) (Apply Exp q i)) 
          o
        side i o = Apply Times 
           (form a b i o) (form c d o i)
    in  (2, ( side x y , side y x ))

data Op = Plus | Times | Exp deriving Show
data E = One | Proj Int
       | Apply Op E E 
    deriving Show


inter :: Fun B.Boolean
      -> Fun B.Boolean
      -> Fun B.Boolean
      -> Int -> Int 
      -> E
      -> Satchmo.SAT.Mini.SAT (Fun B.Boolean)
inter plus times exp d k e = case e of
    One -> constant d k 1
    Proj i -> projection d k i
    Apply op l r -> do
        [x,y] <- forM [l,r] $ inter plus times exp d k
        substitute ( case op of
            Plus -> plus ; Times -> times; Exp -> exp
          ) [x,y]

-- | representation on k-ary functions
-- on finite omain [ 1 .. dom ]:
-- m [y, x1, .., xk ] iff  y = f(x1,..,xk) .
-- this includes the special case k=0 (constants).
data Fun b = 
     Fun { dom :: Int, arity :: Int
         , contents :: M.Map [ Int ] b
         }
    deriving Show

apply :: Fun b -> [Int] -> Int -> b
apply f xs y = contents f M.! ( y : xs )

instance C.Decode m b c 
    => C.Decode m (Fun b) (Fun c) where
        decode f = do
            c <- C.decode $ contents f
            return $ f { contents = c }

constant d k c = do
    f <- mkfun d k
    forM (M.toList $ contents f) $ \ ((y:xs),v) ->do
        when (y == c) $
            B.assert [ B.not $ apply f xs y, v ]
    return f

-- note: leftmost is projection d k 1,
-- rightmost is projectino d k k
projection d k i = do
    f <- mkfun d k
    forM_ (M.toList $ contents f ) $ \ ((y:xs), v) -> 
        B.assert [ B.not $ apply f xs y, v ]
    return f

mkfun :: B.MonadSAT m
      => Int -- ^ domain
      -> Int -- ^ arity
      -> m (Fun B.Boolean)
mkfun d k = do
    pairs <- 
      forM (sequence $ replicate (k+1) [1..d]) $ \ i ->
        do v <- B.boolean; return (i, v)
    let c = M.fromList pairs  
        f = Fun { dom = d, arity = k , contents = c }
    forM_ ( sequence $ replicate k [1..d] ) $ \ xs -> do
        e <- C.exactly 1 $ for [1..d] $ \ y ->
            apply f xs y
        B.assert [e]
    return f

equals f g = do
    oks <- forM ( M.keys $ contents f ) $ \ (y:xs) ->
        B.equals2 (apply f xs y) (apply g xs y)
    B.and oks

substitute :: B.MonadSAT m
           => Fun B.Boolean
           -> [ Fun B.Boolean ]
           -> m (Fun B.Boolean)
substitute f gs = do
    let d = dom f
        k = arity $ head gs
    h <- mkfun d k
    forM_ (sequence $ replicate (k+1) [1..d]) $ \ (y : xs) -> do
        forM_ (sequence $ replicate (length gs) [1..d] ) $ \ zs -> do
            B.assert $ apply h xs y
                  : B.not (apply f zs y)
                  : ( for (zip zs gs) $ \ (z,g) -> 
                       B.not (apply g xs z) )
    return h

for = flip map
