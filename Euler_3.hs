{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Euler_3 where

import Numeric.Natural ( Natural )
import Data.Functor.Foldable ( Recursive(cata) )


-- | >>> euler_3 600851475143
-- 6857
euler_3 :: Natural -> Natural
euler_3 x = extractAns $ cata f $ prepareNum x where
    extractAns = \(d, y, z) -> d
    prepareNum :: Natural -> Natural
    prepareNum = ceiling . sqrt . fromIntegral
    f Nothing = (x, 2, False)
    f (Just (n, d, True)) = (n, d, True)
    f (Just (n, d, _x))
        | n == d         = (d, d, True)
        | n `mod` d == 0 = (n `div` d, d, False)
        | otherwise      = (n, d + 1, False)
