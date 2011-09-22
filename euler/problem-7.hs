-- | Solution to Project Euler problem #7
-- | SOLVED
-- | There really should be a better way to do this...

main :: IO ()
main = do
    let primes = primesTo 150000
    let ans = primes !! 10000
    putStrLn $ show ans

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
