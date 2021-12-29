{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Euler_12 where

import GHC.Natural
import Data.Functor.Foldable


-- infList = [(), ()..]

-- | >>> triangleNums 10
-- [55,45,36,28,21,15,10,6,3,1,0]

-- triangleNums :: Int -> [Int]
-- triangleNums n = cata f $ take n infList where
--     f Nil = [0]
--     f (Cons _ a) = head a + length a : a

nextTriNum :: [Int] -> [Int]
nextTriNum a = head a + length a : a

-- | >>> primeDivList 1000
-- [5,5,5,2,2,2]
primeDivList :: Natural -> [Natural]
primeDivList x = extractAns $ cata f $ prepareNum x where

    extractAns = \(d, y, z, arr) -> if z then arr else d:arr
    prepareNum :: Natural -> Natural
    prepareNum = ceiling . sqrt . fromIntegral

    f Nothing = (x, 2, False, [])
    f (Just (n, d, True, arr)) = (n, d, True, arr)

    f (Just (n, d, _, arr))
        | n == d         = (d, d, True, d:arr)
        | n `mod` d == 0 = (n `div` d, d, False, d:arr)
        | otherwise      = (n, d + 1, False, arr)

checkAns :: Natural -> Bool
checkAns num = (>= 500) $ length $ rmdups $ concat [map product (subsequencesOfSize x $ primeDivList num) |
   x <- [1 .. length $ primeDivList num]]

rmdups :: (Eq a) => [a] -> [a]
rmdups = para \case
    Nil                 -> []
    (Cons x (past, xs)) -> if x `elem` past then xs else x:xs




-- | >>> subsequencesOfSize 3 "abcd"
-- ["bcd","acd","abd","abc"]
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if n > l then [] else subsequencesBySize xs !! (l-n)
 where
   subsequencesBySize [] = [[[]]]
   subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                             in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])
                             
-- | >>> euler_12 [1, 0]
-- 76576500
euler_12 arr = if checkAns $ intToNatural $ head arr then head arr else euler_12 $ nextTriNum arr