module Replicatetest where

f :: Int -> [Int] -> [Int]
f n (x:[]) = replicate n x
f n (x:xs) = (replicate n x) ++ (f n xs)
