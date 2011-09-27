-- | Solution to Project Euler problem #10
-- | SOLVED

main :: IO ()
main = do
    let ans = sumPrimesBelow 2000000
    putStrLn $ show ans

sumPrimesBelow :: Integer -> Integer
sumPrimesBelow max = f primes
    where f (x:xs) = if x < max
                         then x + f xs
                         else 0

primes :: [Integer]
primes = 2 : 3 : sieve (tail primes) [5,7..] where
  sieve (p:ps) xs
        = h ++ sieve ps [x|x<-t, x `rem` p /= 0]
           where (h,(_:t)) = span (< p*p) xs

