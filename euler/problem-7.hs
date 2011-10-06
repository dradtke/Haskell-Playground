-- | Solution to Project Euler problem #7
-- | SOLVED

import Primes

main :: IO ()
main = do
    let ans = primes !! 10000
    putStrLn $ show ans

