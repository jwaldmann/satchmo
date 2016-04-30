{-# language FlexibleContexts #-}

module Satchmo.Numeric where

import Satchmo.Boolean
import Satchmo.Code

class Constant a where
    constant :: MonadSAT m => Integer -> m a
    
class Create a where    
    -- | Parameter: bit width
    create :: MonadSAT m => Int -> m a 
    
class Numeric a where
    equal :: MonadSAT m => a -> a -> m Boolean
    greater_equal :: MonadSAT m => a -> a -> m Boolean
    plus :: MonadSAT m => a -> a -> m a
    minus :: MonadSAT m => a -> a -> m a
    times :: MonadSAT m => a -> a -> m a
    
