{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Euler_5 where

import Numeric.Natural ( Natural )
import Data.Functor.Foldable ( Recursive(para) )

-- | >>> euler_5 20
-- 232792560
euler_5 :: Natural -> Natural 
euler_5 = para f where
    f Nothing = 1
    f (Just (x, n)) =
        if (n `mod` (x+1)) == 0
        then n
        else head $ filter (\b -> mod b (x+1) == 0) (fmap (n*) [1..(x+1)])
