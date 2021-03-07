-- Числа Фиббоначе, бесконечный список
-- [1, 1, 2, 3, 5, 8, 13, ...]

fib :: Num n => [n]
fib = 0 : nxt
    where nxt = 1 : zipWith (+) fib nxt

-- main :: IO ()
-- main = do
--     print(take 9 fib)