{--
PROBLEM 1:

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
--}

-- First Implementation: Monolithic

-- Tail recursion
f1 :: Integral t => t -> t
f1 n = helper n 0
    where helper 0 acc = acc
          helper n acc = helper (n-1) (acc + if n `mod` 5 == 0 || n `mod` 3 == 0 then n else 0)

-- test : f1 9

-- recursion
f2 :: Integral p => p -> p
f2 0 = 0
f2 n = f1 (n-1) + if n `mod` 5 == 0 || n `mod` 3 == 0 then n else 0

-- test: f2 9




--- MODULAR IMPLEMENTATION

f3 :: Integral b => b -> b
f3 n = foldl (+) 0 filtered
    where filtered = filter (\x -> x `mod` 5 == 0 || x `mod` 3 == 0) numbers
          numbers = [1..n]

--- SPECIAL LIST SYNTAX

f4 :: Integral a => a -> a
f4 n = sum [x | x <- [1..n], x `mod` 5 == 0 || x `mod` 3 == 0]




    
-- INFINITE LISTS

f5 :: Integral a => Int -> a
f5 n = sum $ filter (\x -> x `mod` 5 == 0 || x `mod` 3 == 0) numbers
    where numbers = take (n+1) [1..]


--- Using a map

f6 :: Integral a => a -> a
f6 n = sum $ (map (*3) [1..n `div` 3]) ++ (map (*5) [1..n `div` 5]) ++ (map ((-15)*) [1..n `div` 15])
          

