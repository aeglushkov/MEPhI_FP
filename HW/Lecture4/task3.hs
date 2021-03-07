-- Числа Фибоначчи
fib :: (Eq p, Num p) => p -> p
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Обращение списка
reverse' :: [a] -> [a]
reverse' [x] = [x]
reverse' (x:xs) = reverse' xs ++ [x]

-- main :: IO ()
-- main = do
--     let n = 10
--     print(fib n)

--     let list = [1, 2, 3, 4, 5]
--     print(reverse' list)