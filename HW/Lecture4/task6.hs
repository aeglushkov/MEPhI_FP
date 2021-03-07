-- firstElem — функция, возвращающая номер первого вхождения заданного атома в список.
firstElem :: (Eq t2, Num t1) => t2 -> [t2] -> t1
firstElem el list = let
    firstElem' k el [] = 0
    firstElem' k el [x] = if el == x then k else 0
    firstElem' k el (x:xs) = if el == x then k else firstElem' (k + 1) el xs
    in firstElem' 1 el list

-- set — функция, возвращающая список из всех атомов, содержащихся в заданном списке. Каждый атом должен присутствовать в результирующем списке в единственном числе.
set :: Eq a => [a] -> [a]
set list = let
    set' setList [x] = if firstElem x setList == 0 then setList ++ [x] else setList
    set' setList (x:xs) = if firstElem x setList == 0 then set' (setList ++ [x]) xs else set' setList xs
    in set' [] list

-- Подсчитывает количество вхождений элемента в список
countOfEl :: (Eq t1, Num t2) => t2 -> t1 -> [t1] -> [(t1, t2)]
countOfEl cnt el [x] = if el == x then [(el, cnt + 1)] else [(el, cnt)]
countOfEl cnt el (x:xs) = if el == x then countOfEl (cnt + 1) el xs else countOfEl cnt el xs

-- freq — функция, возвращающая список пар (символ, частота). Каждая пара определяет атом из заданного списка и частоту его вхождения в этот список.
freq :: (Eq t1, Num t2) => [t1] -> [(t1, t2)]
freq list = let
    freq' [] list zip = zip
    freq' (x:xs) list zip = freq' xs list (zip ++ countOfEl 0 x list)
    in freq' (set list) list []

main :: IO ()
main = do
    print(set "Hello world!")
    print(freq "Hello world!")