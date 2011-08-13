-- | PolishCalculator.hs
-- | -------------------
-- | Solves mathematical expressions written in reverse polish notation,
-- | e.g. 10 4 3 + 2 * -    = -4

import Data.List

-- | The main method just repeatedly takes input,
-- | exiting once blank input is provided
main :: IO ()
main = do input <- getLine
          if null input
              then return ()
              else do
                  let answer = solveRPN input
                  putStrLn $ "Answer = " ++ show answer
                  main

-- | Takes a string representing an expression and returns its result
solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:rest) "*"    = (x*y):rest
          foldingFunction (x:y:rest) "+"    = (x+y):rest
          foldingFunction (x:y:rest) "-"    = (y-x):rest
          foldingFunction (x:y:rest) "/"    = (y/x):rest
          foldingFunction (x:y:rest) "^"    = (y**x):rest
          foldingFunction (x:rest) "ln"     = (log x):rest
          foldingFunction rest "sum"        = [sum rest]
          foldingFunction rest numberString = read numberString:rest

