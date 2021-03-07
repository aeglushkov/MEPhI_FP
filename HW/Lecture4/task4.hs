-- Напишите рекурсивную функцию, проверяющую балансировку скобок в строке (Как вы должны помнить, строки в Haskell - это список символов [Char]).

-- Например, для следующих строк функция должна вернуть True:
-- (if (zero? x) max (/ 1 x))
-- I told him (that it’s not (yet) done). (But he wasn’t listening)

-- А для строк ниже, напротив, функция должна вернуть значение False:
-- :-)
-- ())(
-- Последний пример демонстрирует: недостаточно проверить только, что строка содержит равное количество открывающих и закрывающих скобок.

-- balance :: String → Bool -- функция из строки в логическое значение

balance' :: Int -> String -> Bool
balance' n [x] 
    | x == '(' = False 
    | x == ')' = n == 1
    | n == 0 = True
    | otherwise = False
balance' n (x:xs) 
    | x == '(' = balance' (n + 1) xs 
    | x == ')' = if n > 0 then balance' (n - 1) xs else False
    | otherwise = balance' n xs

balance :: String -> Bool
balance str = do
    let n = 0
    balance' n str

main :: IO ()
main = do
    let str1 = "(if (zero? x) max (/ 1 x))"
    let str2 = "I told him (that it’s not (yet) done). (But he wasn’t listening)"
    let str3 = ":-)"
    let str4 = "())("

    print(balance str1)
    print(balance str2)
    print(balance str3)
    print(balance str4)
