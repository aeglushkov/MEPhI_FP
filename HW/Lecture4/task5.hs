-- reverseAll — функция, получающая на вход списочную структуру и обращающая все её элементы, а также её саму.
reverse' :: [a] -> [a]
reverse' [x] = [x]
reverse' (x:xs) = reverse' xs ++ [x]

reverseAll :: [[a]] -> [[a]]
reverseAll [x] = [reverse' x]
reverseAll (x:xs) = reverseAll xs ++ [reverse' x]

-- firstElem — функция, возвращающая номер первого вхождения заданного атома в список.
firstElem' k el [x] = if el == x then k else 0
firstElem' k el (x:xs) = if el == x then k else firstElem' (k + 1) el xs

firstElem el list = do
    let n = 1
    firstElem' n el list

-- main :: IO ()
-- main = do
--     print(reverseAll [[1,2,3], [4, 5, 6], [7, 8, 9]])
--     print(firstElem 'l' "Hello world!")