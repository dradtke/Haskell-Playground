-- | BruteQueens.hs
-- | --------------
-- | A brute-force Haskell solution for the generic NxN queens puzzle.
-- | Exactly the same as Queens.hs, though much less efficient.

import System.Environment

type Queen = (Int,Int)

main :: IO ()
main = do
    args <- getArgs
    let n = case args of [] -> 8
                         (a:rgs) -> read a :: Int
    putStrLn $ show (solveQueens n)

solveQueens :: Int -> Maybe [Queen]
solveQueens n
    | n < 2 = Nothing
    | otherwise = solveQueens' n []

solveQueens' :: Int -> [Queen] -> Maybe [Queen]
solveQueens' n stack
    | x < 1 = if isValid stack then Just stack else Nothing
    | otherwise = foldl loop Nothing [1..n]
    where x = n - length stack
          loop (Just s) y = Just s
          loop Nothing  y = solveQueens' n $ (x,y):stack

isValid :: [Queen] -> Bool
isValid stack = foldl validate True pairs
    where pairs = [(s1,s2) | s1 <- stack, s2 <- stack, s1 /= s2]
          validate False _ = False
          validate True ((x1,y1), (x2,y2)) =
              x1 /= x2 && y1 /= y2 && (abs $ y2 - y1) /= (abs $ x2 - x1)
