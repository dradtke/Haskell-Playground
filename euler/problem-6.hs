-- | Solution to Project Euler problem #6
-- | SOLVED

main :: IO ()
main = do
    let range = [1..100]
    let ans = (squareOfSum range) - (sumOfSquares range)
    putStrLn $ show ans

sumOfSquares :: [Integer] -> Integer
sumOfSquares x = sum $ map (^2) x

squareOfSum :: [Integer] -> Integer
squareOfSum x = (sum x) ^ 2
