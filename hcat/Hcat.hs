-- | Hcat.hs
-- | -------
-- | Haskell cat. Takes a list of files as arguments and
-- | prints their contents.

import Control.Monad
import System.Environment

main :: IO ()
main = do args <- getArgs
          -- Make sure something was supplied to arguments
          when (null args) $ error "No files were supplied"
          catAll args

-- | Takes a list of file names and displays each one in turn, appending an
-- | additional newline to all but the last
catAll :: [String] -> IO ()
catAll [] = return ()
catAll (x:rest) = catch (do cat x $ (length rest) /= 0 ; catAll rest) handler
    where handler e = putStrLn $ show e
          cat f e = do contents <- readFile f
                       putStrLn $ "-- " ++ f ++ " --"
                       (if e then putStrLn else putStr) contents
