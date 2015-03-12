{-# language TemplateHaskell #-}

import qualified Satchmo.Boolean as B
import Satchmo.SAT.Mini ( solve )
import Satchmo.Code ( decode )

import Satchmo.TH ( compile )

test1 = do
  out <- solve $ do
    x <- B.boolean ; y <- B.boolean
    z <- $(compile [| and [x,y] |])
    B.assert [z]
    return $ decode (x,y)
  print (out :: Maybe (Bool,Bool))
  


