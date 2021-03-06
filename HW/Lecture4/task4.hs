-- Напишите рекурсивную функцию, проверяющую балансировку скобок в строке (Как вы должны помнить, строки в Haskell - это список символов [Char]).

-- Например, для следующих строк функция должна вернуть True:
-- (if (zero? x) max (/ 1 x))
-- I told him (that it’s not (yet) done). (But he wasn’t listening)

-- А для строк ниже, напротив, функция должна вернуть значение False:
-- :-)
-- ())(
-- Последний пример демонстрирует: недостаточно проверить только, что строка содержит равное количество открывающих и закрывающих скобок.

-- balance :: String → Bool -- функция из строки в логическое значение

balance :: Int -> Int -> String -> Bool
balance left right [x] 
    | x == '(' = False 
    | x == ')' = left == (right + 1)
    | left == right = True
    | otherwise = False
balance left right (x:xs) 
    | x == '(' = balance (left + 1) right xs 
    | (x == ')') && (left >= (right + 1)) = balance left (right + 1) xs
    | (x == ')') && (left < (right + 1)) = False
    | otherwise = balance left right xs

left :: Int
left = 0

right :: Int
right = 0

main = do
    let str1 = "(if (zero? x) max (/ 1 x))"
    let str2 = "I told him (that it’s not (yet) done). (But he wasn’t listening)"
    let str3 = ":-)"
    let str4 = "())("
    print(balance left right str1)
    print(balance left right str2)
    print(balance left right str3)
    print(balance left right str4)
