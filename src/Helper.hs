module Helper where

import Data.Either
import Data.List (sortBy)
import Data.Function ( on )

integralLog :: (Show a, Integral a, Integral b)
    => a -> a -> Either [Char] b
integralLog base x
    | x < 1 || base <= 1 = Left "Invalid input"
    | x == 1 = Right 0
    | y * base == x = 
        if isLeft result
            then Left $ show x ++ " is not a power of " ++ show base
            else (+1) <$> result
    | otherwise = Left $ show x ++ " is not a power of " ++ show base
    where 
        y = quot x base
        result = integralLog base y

sortOnFst :: (Ord a) => [(a, b)]
    -> [(a, b)]
sortOnFst = sortBy (compare `on` fst)

sortOnSnd :: (Ord b) => [(a, b)]
    -> [(a, b)]
sortOnSnd = sortBy (compare `on` snd)