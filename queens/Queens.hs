-- | Queens.hs
-- | ---------
-- | A Haskell solution for the generic NxN queens puzzle.
-- | The goal is to place N queens on an NxN board such that
-- | no two queens are attacking each other, which means they
-- | cannot share the same row, column, or diagonal.
-- |
-- | The solution is represented as a column-indexed list of
-- | rows, so the 4x4 solution of [1,3,0,2] is equivalent
-- | to (0,1), (1,3), (2,0), and (3,2).
-- |
-- | When called with no arguments, it calculates a solution
-- | for N = 8. Other values of N can be passed in as the
-- | first command-line argument.

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let n = case args of [] -> 8
                         (a:rgs) -> read a :: Int
    let result = solveQueens n
    putStrLn $ show result

solveQueens :: Int -> Maybe ([Int])
solveQueens n = queens n []

queens :: Int -> [Int] -> Maybe ([Int])
queens n stack
    | n < 4             = Nothing
    | length stack == n = Just stack
    | otherwise         = loop 0
    where loop i = if i >= n
              then Nothing
              else if isValid stack i
                  then case queens n (stack ++ [i]) of
                       Just s  -> Just s
                       Nothing -> loop (i+1)
                  else loop (i+1)
                    
isValid :: [Int] -> Int -> Bool
isValid [] _ = True
isValid stack element = f stack 0
    where len = length stack
          f [] _ = True
          f (s:tack) y
              | s == element                           = False
              | (abs $ element - s) == (abs $ y - len) = False
              | otherwise                              = f tack (y+1)
