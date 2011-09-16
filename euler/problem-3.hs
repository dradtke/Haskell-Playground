-- | Solution to Project Euler problem #3

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
        primeList = reverse $ primesToNA $ rootFloor x
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
    where primeList = primesToNA $ rootFloor x
          f x (p:rest) = if (mod x p) == 0
                             then False
                             else f x rest
          f _ _ = True

-- | No fucking idea how this thing works, but it does.
primesToNA n = 2: [i | i <- [3,5..n], ar ! i]
   where
    ar = f 5 $ accumArray (\ a b -> False) True (3,n) 
                        [(i,()) | i <- [9,15..n]]
    f p a | q > n = a
          | True  = if null x then a' else f (head x) a'
      where q = p*p
            a'= a // [(i,False) | i <- [q,q+2*p..n]]
            x = [i | i <- [p+2,p+4..n], a' ! i]
