-- to make it really fast, use libraries :P
-- import Data.Numbers.Primes

-- primality testing
-- factoring a number
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime n = factors n == [1,n]

-- very inefficient
primeFactors :: Int -> [Int]
primeFactors n = [x | x <- factors n, isPrime x]

-- subset
-- O(n^2) implementation
isSubset :: Eq a => [a] -> [a] -> Bool
isSubset xs ys = foldr (\f acc -> f ys && acc) True $ map elem xs

-- the final function
hamming :: Int -> [Int]
hamming n = take n $ [x | x <-[1..], primeFactors x `isSubset` [2,3,5]]

-- input
main :: IO ()
main = do
  putStrLn "How many Hamming numbers do you want to generate?"
  n <- getLine
  putStrLn "Processing..."
  putStrLn $ show $ hamming $ read n
