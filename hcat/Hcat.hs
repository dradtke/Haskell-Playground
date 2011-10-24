-- | Hcat.hs
-- | -------
-- | Haskell cat. Takes a list of files as arguments and
-- | prints their contents.

import System.Directory
import System.Environment
import System.IO
import System.IO.Error

main :: IO ()
main = do args <- getArgs
          -- Make sure something was supplied to arguments
          if null args
              then error "No files were supplied"
              else catAll args

-- | Takes a list of file names and calles 'catFile' on each one in turn
catAll :: [String] -> IO ()
catAll (x:rest) = do catFile' x $ (length rest) /= 0
                     catAll rest
catAll x = return ()

-- | Prints a file's contents, with or without an additional newline
catFile' :: String -> Bool -> IO ()
catFile' fileName extraNewline = 
    catch (do contents <- readFile fileName
              show' contents)
          (\e -> show' $ ioeGetErrorString e ++ "\n")
    where show' str = do
              putStrLn $ "-- " ++ fileName ++ " --"
              (if extraNewline then putStrLn else putStr) str

-- | Prints a file's contents with no additional newline
catFile :: String -> IO ()
catFile fileName = catFile' fileName False

