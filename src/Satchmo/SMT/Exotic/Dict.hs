module Satchmo.SMT.Exotic.Dict where

import Satchmo.SMT.Exotic.Domain

data Dict m e b = Dict 
    { info :: String
    , domain :: Domain
    , fresh :: m e
    , finite :: e -> m b
    , gg :: e -> e -> m b
    , ge :: e -> e -> m b
    , plus :: [e] -> m e
    , times :: [e] -> m e
    }



