-- reverseAll — функция, получающая на вход списочную структуру и обращающая все её элементы, а также её саму.
reverse' :: [a] -> [a]
reverse' [x] = [x]
reverse' (x:xs) = reverse' xs ++ [x]

reverseAll :: [[a]] -> [[a]]
reverseAll [x] = [reverse' x]
reverseAll (x:xs) = reverseAll xs ++ [reverse' x]

-- firstElem — функция, возвращающая номер первого вхождения заданного атома в список.
firstElem :: (Eq t1, Num t2) => t1 -> [t1] -> t2
firstElem el list = let
    firstElem' k el [x] = if el == x then k else 0
    firstElem' k el (x:xs) = if el == x then k else firstElem' (k + 1) el xs
    in firstElem' 1 el list

main :: IO ()
main = do
    print(reverseAll [[1,2,3], [4, 5, 6], [7, 8, 9]])
    print(firstElem 'l' "Hello world!")
    print(firstElem 1 [1, 2, 3])