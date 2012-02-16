-- | Queens.hs
-- | ---------
-- | A Haskell solution for the generic NxN queens puzzle.
-- | The goal is to place N queens on an NxN board such that
-- | no two queens are attacking each other, which means they
-- | cannot share the same row, column, or diagonal.
-- |
-- | When called with no arguments, it calculates a solution
-- | for N = 8. Other values of N can be passed in as the
-- | first command-line argument.

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
    | n < 4 = Nothing
    | otherwise = queens n []

queens :: Int -> [Queen] -> Maybe [Queen]
queens n stack
    | x < 1 = Just stack
    | otherwise = foldl loop Nothing [1..n]
    where x = n - length stack
          loop (Just s) y = Just s
          loop Nothing  y =
              if isValid stack (x,y)
                  then queens n $ (x,y):stack
                  else Nothing
                    
isValid :: [Queen] -> Queen -> Bool
isValid stack (x2,y2) = foldl validate True stack
    where validate False _ = False
          validate True (x1,y1) =
              x1 /= x2 && y1 /= y2 && (abs $ y2 - y1) /= (abs $ x2 - x1)
