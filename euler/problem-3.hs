-- | Solution to Project Euler problem #3
-- | SOLVED

import Array

main :: IO ()
main = do
    let answer = getLargestPrimeFactor 600851475143
    putStrLn $ show answer

-- | Calculates the largest prime factor of x
-- | Works by first finding a reverse-ordered list of
-- | all primes less than the square root of x, then
-- | returns the first of those that is a factor of x
getLargestPrimeFactor :: Integer -> Integer
getLargestPrimeFactor x = f x primeList
    where
        primeList = reverse $ primesTo $ rootFloor x
        f x (p:rest) = if (mod x p) == 0
                           then p
                           else f x rest
        f _ _ = 1

-- | Returns the floor of the square root of x
rootFloor :: Integer -> Integer
rootFloor x = floor $ sqrt $ fromIntegral x

-- | Returns true if x is prime, false otherwise
isPrime :: Integer -> Bool
isPrime x = f x primeList
    where primeList = primesTo $ rootFloor x
          f x (p:rest) = if (mod x p) == 0
                             then False
                             else f x rest
          f _ _ = True

-- | I have a slightly better idea of how this one works...
primesTo :: Integer -> [Integer]
primesTo m = 2 : sieve [3,5..m]  where
    sieve []     = []
    sieve (p:xs) = p : sieve (xs `minus` [p*p,p*p+2*p..m])

minus :: (Ord t) => [t] -> [t] -> [t]
minus (x:xs) (y:ys) = case (compare x y) of 
          LT -> x : minus  xs  (y:ys)
          EQ ->     minus  xs     ys 
          GT ->     minus (x:xs)  ys
minus  xs     _     = xs
