module Satchmo.SMT.Exotic.Natural where

import Prelude hiding ( not, and, or )

import Satchmo.SMT.Exotic.Dict
import qualified Satchmo.SMT.Exotic.Domain

import qualified Satchmo.SAT.Mini
import qualified Satchmo.Boolean as B

import qualified Satchmo.Unary.Op.Fixed
import qualified Satchmo.Unary.Op.Flexible
import qualified Satchmo.Unary as Un

import qualified Satchmo.Binary as Bin
import qualified Satchmo.Binary.Op.Fixed  
import qualified Satchmo.Binary.Op.Flexible

import Control.Monad ( foldM )

data Encoding = Unary | Binary deriving Show
data Unary_Addition = Odd_Even_Merge | Bitonic_Sort | Quadratic deriving Show
data Extension = Fixed | Flexible deriving Show

unary_fixed :: Int -> Unary_Addition 
            -> Dict Satchmo.SAT.Mini.SAT Un.Number B.Boolean
unary_fixed bits a = Dict
    { info = unwords [ "unary", "bits:", show bits, "(fixed)", "addition:", show a ]
    , domain = Satchmo.SMT.Exotic.Domain.Natural
    , fresh = Un.number bits
    , plus = foldM1 $ case a of
          Quadratic -> Satchmo.Unary.Op.Fixed.add_quadratic
          Bitonic_Sort -> Satchmo.Unary.Op.Fixed.add_by_bitonic_sort
          Odd_Even_Merge -> Satchmo.Unary.Op.Fixed.add_by_odd_even_merge
    , gg = Un.gt
    , ge = Un.ge
    }

unary_flexible :: Int -> Unary_Addition
               -> Dict Satchmo.SAT.Mini.SAT Un.Number B.Boolean
unary_flexible bits a = Dict
    { info = unwords [ "unary", "bits:", show bits, "(flexible)", "addition:", show a ]
    , domain = Satchmo.SMT.Exotic.Domain.Natural
    , fresh = Un.number bits
    , plus = foldM1 $ case a of
          Quadratic -> Satchmo.Unary.Op.Flexible.add_quadratic
          Bitonic_Sort -> Satchmo.Unary.Op.Flexible.add_by_bitonic_sort
          Odd_Even_Merge -> Satchmo.Unary.Op.Flexible.add_by_odd_even_merge
    , gg = Un.gt
    , ge = Un.ge
    }


binary_fixed :: Int -> Dict Satchmo.SAT.Mini.SAT Bin.Number B.Boolean
binary_fixed bits = Dict
    { info = unwords [ "binary", "bits:", show bits, "(fixed)" ]
    , domain = Satchmo.SMT.Exotic.Domain.Natural
    , fresh = Bin.number bits
    , plus = foldM1 $ Satchmo.Binary.Op.Fixed.add
    , times = foldM1 $ Satchmo.Binary.Op.Fixed.times
    , gg = Bin.gt
    , ge = Bin.ge
    , finite = \ n -> B.or $ Bin.bits n
    }


binary_flexible :: Int -> Dict Satchmo.SAT.Mini.SAT Bin.Number B.Boolean
binary_flexible bits = Dict
    { info = unwords [ "binary", "bits:", show bits, "(flexbible)" ]
    , domain = Satchmo.SMT.Exotic.Domain.Natural
    , fresh = Bin.number bits
    , plus = foldM1 $ Satchmo.Binary.Op.Flexible.add
    , times = foldM1 $ Satchmo.Binary.Op.Flexible.times
    , gg = Bin.gt
    , ge = Bin.ge
    , finite = \ n -> B.or $ Bin.bits n
    }

foldM1 :: Monad m => ( b -> b -> m b ) -> [b] -> m b
foldM1 f (x:xs) = foldM f x xs
