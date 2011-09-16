-- | Solution to Project Euler problem #2

main :: IO ()
main = do
    let fibs = fibonacci 1 2 4000000
    let evens = getEvens fibs
    putStrLn $ show (sum evens)

-- | Calculates a fibonacci sequence
-- | Requires a first number, second number, and maximum (stopping point)
fibonacci :: Integer -> Integer -> Integer -> [Integer]
fibonacci first second max =
    if first >= max
        then []
        else first : fibonacci second (first + second) max

-- | Filters out all odd numbers from the given list
getEvens :: [Integer] -> [Integer]
getEvens (x:rest) =
    if (mod x 2) == 0
        then x : (getEvens rest)
        else getEvens rest
getEvens x = []
