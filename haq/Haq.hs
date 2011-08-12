-- | Haq.hs
-- | ------
-- | A modified hello world. Prints the first argument
-- | prepended with "Haq! "

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ haqify (head args)

haqify s = "Haq! " ++ s
