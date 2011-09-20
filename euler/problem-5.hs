-- | Solution to Project Euler problem #5
-- | SOLVED - however, it is rather slow

main :: IO ()
main = do
    putStrLn $ show $ findSmallestDivisor [1..20]

-- | Returns true if x divides evenly by every element
-- | in the list
dividesByAll :: Integer -> [Integer] -> Bool
dividesByAll x range = f x range
    where f x (y:rest) = if mod x y == 0 then f x rest else False
          f _ _        = True

-- | Given a list of numbers, finds the smallest integer that
-- | divides evenly by all of them
findSmallestDivisor :: [Integer] -> Integer
findSmallestDivisor range = f 1
    where f x = if dividesByAll x range then x else f (x + 1)
