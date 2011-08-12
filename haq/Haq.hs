import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ haqify (head args)

haqify s = "Haq! " ++ s
