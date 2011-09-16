-- | List-printing convenience method
-- | Not actually useful anymore

module ListPrinter
( printList
) where

printList :: (Show a) => [a] -> IO ()
printList (x:rest) = do
    putStrLn (show x)
    printList rest
printList x = return ()
