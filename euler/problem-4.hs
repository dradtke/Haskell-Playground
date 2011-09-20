-- | Solution to Project Euler problem #4
-- | SOLVED

main :: IO ()
main = do
    let ans = findLargestPalindrome 999 100
    putStrLn $ show ans

-- | Returns true if x is a palindromic number
isPalindrome :: Integer -> Bool
isPalindrome x = pre == (reverse post)
    where str_x = show x
          x_length = length str_x
          split = div x_length 2
          pre = fst $ splitAt split str_x
          post = snd $ splitAt (x_length - split) str_x

-- | Finds the largest palindromic number that is a product
-- | of two numbers within the given range
-- | Not very fast, it just brute forces it
findLargestPalindrome :: Integer -> Integer -> Integer
findLargestPalindrome high low = f high high 0
    where f x y ans
              | y >= low && x >= low = if isPalindrome product
                                          then f x (y - 1) (max product ans)
                                          else f x (y - 1) ans
              | x > low   = f (x - 1) high ans
              | otherwise = ans
              where product = x * y

