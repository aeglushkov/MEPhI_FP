-- Список треугольных чисел
countTriangle :: Int->[Int]
countTriangle 1 = [1]
countTriangle x 
    | x == 1 = [1]
    | otherwise = countTriangle (x-1) ++ [x*(x+1) `div` 2]

-- Список пирамидальных чисел
countPyr::Int->[Int]
countPyr x
    | x == 0 = []
    | otherwise = countPyr (x-1) ++ [sum (countTriangle x)]
    