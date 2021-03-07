-- firstElem — функция, возвращающая номер первого вхождения заданного атома в список.
firstElem' k el [] = 0
firstElem' k el [x] = if el == x then k else 0
firstElem' k el (x:xs) = if el == x then k else firstElem' (k + 1) el xs

firstElem el list = do
    let n = 1
    firstElem' n el list

-- set — функция, возвращающая список из всех атомов, содержащихся в заданном списке. Каждый атом должен присутствовать в результирующем списке в единственном числе.
set' setList [x] = if firstElem x setList == 0 then setList ++ [x] else setList
set' setList (x:xs) = if firstElem x setList == 0 then set' (setList ++ [x]) xs else set' setList xs

-- freq — функция, возвращающая список пар (символ, частота). Каждая пара определяет атом из заданного списка и частоту его вхождения в этот список.

-- Подсчитывает количество вхождений элемента в список
countOfEl cnt el [x] = if el == x then [(el, cnt + 1)] else [(el, cnt)]
countOfEl cnt el (x:xs) = if el == x then countOfEl (cnt + 1) el xs else countOfEl cnt el xs

freq' [] list zip = zip
freq' (x:xs) list zip = freq' xs list (zip ++ countOfEl 0 x list)

freq list = freq' (set' [] list) list []

-- main :: IO ()
-- main = do
--     print(set' [] "Hello world!")
--     print(freq "Hello world!")