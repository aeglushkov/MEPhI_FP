-- Числа Фиббоначе, бесконечный список
-- [1, 1, 2, 3, 5, 8, 13, ...]
fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
    print(take 40 fibs)