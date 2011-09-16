-- | Solution to Project Euler problem #1
-- | SOLVED

main :: IO ()
main = do
    let list = getThreesAndFives 1000
    putStrLn $ show (sum list)

-- | Given a starting integer, returns all positive integers
-- | less than it that are multiples of either 3 or 5
getThreesAndFives :: Integer -> [Integer]
getThreesAndFives x
    | y <= 0                           = []
    | (mod y 3) == 0 || (mod y 5) == 0 = y : getThreesAndFives y
    | otherwise                        = getThreesAndFives y
    where y = x - 1
