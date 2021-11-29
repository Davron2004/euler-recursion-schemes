{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Euler_6 where

import Numeric.Natural ( Natural )
import Data.Functor.Foldable

listDownToOne n = enumFromThenTo n (n - 1) 1

sumOfSquares :: Natural -> Natural
sumOfSquares = sum . fmap (^2) . listDownToOne

squareOfSum :: Natural -> Natural
squareOfSum = (^2) . sum . listDownToOne

-- | >>> diff 100
-- 25164150
diff :: Natural -> Natural
diff n = squareOfSum n - sumOfSquares n
