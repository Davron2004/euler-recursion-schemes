{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Euler_1 where

import GHC.Natural ( Natural )
import Data.Functor.Foldable ( Recursive(para) )

-- | >>> euler_1 3 5 1000
-- 233168
euler_1 :: Natural -> Natural -> Natural -> Natural
euler_1 n m = para \case
    Nothing     -> 0
    Just (x, a) ->
        if (x `mod` n == 0) || (x `mod` m == 0)
        then x + a
        else a
