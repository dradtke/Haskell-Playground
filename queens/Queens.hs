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
-- |
-- | An alternative solution for testing the diagonal was attempted:
-- |   (y2 - y1) + x1 == x2
-- | but ultimately failed. The solution for queens 8 resulted in
-- | a diagonal conflict between (3,7) and (4,6):
-- |
-- | Just [(0,0),(1,2),(2,5),(3,7),(4,6),(5,3),(6,1),(7,4)]
-- | 

import System.Environment

type Square = (Int,Int)

main :: IO ()
main = do
    args <- getArgs
    let n = case args of [] -> 8
                         (a:rgs) -> read a :: Int
    putStrLn $ show (solveQueens n)

solveQueens :: Int -> Maybe ([Square])
solveQueens n
    | n < 4 = Nothing
    | otherwise = queens (n-1) []

queens :: Int -> [Square] -> Maybe ([Square])
queens n stack
    | x < 0 = Just stack
    | otherwise = foldl loop Nothing [0..n]
    where x = n - length stack
          loop (Just s) i = Just s
          loop Nothing  i =
              if isValid stack (x,i)
                  then queens n $ (x,i):stack
                  else Nothing
                    
isValid :: [Square] -> Square -> Bool
isValid stack (x2,y2) = foldr test True stack
    where test _ False = False
          test (x1,y1) True = (y1 /= y2) && (abs (y2 - y1) /= abs (x2 - x1))
