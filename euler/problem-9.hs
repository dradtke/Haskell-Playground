-- | Solution to Project Euler problem #9
-- | SOLVED

main :: IO ()
main = do
    let ans = getTripleProduct (findTripleWithSum 1000)
    putStrLn $ show ans

-- | A list of all possible pythagorean triples
triples :: [(Integer, Integer, Integer)]
triples = [ (a,b,c) | c <- [1..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]

findTripleWithSum :: Integer -> (Integer, Integer, Integer)
findTripleWithSum target = f triples
    where f (t@(a,b,c):rest) = if (a+b+c == target) then t else f rest

getTripleProduct :: (Integer, Integer, Integer) -> Integer
getTripleProduct (a,b,c) = a*b*c
