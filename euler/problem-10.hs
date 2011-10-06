-- | Solution to Project Euler problem #10
-- | SOLVED

import Primes

main :: IO ()
main = do
    let ans = sumPrimesBelow 2000000
    putStrLn $ show ans

sumPrimesBelow :: Integer -> Integer
sumPrimesBelow max = f primes
    where f (x:xs) = if x < max
                         then x + f xs
                         else 0

