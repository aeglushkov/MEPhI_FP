-- Список треугольных чисел
triangleList :: Int -> [Int]
triangleList 1 = [1]
triangleList x 
    | x == 1 = [1]
    | otherwise = triangleList (x - 1) ++ [x * (x + 1) `div` 2]

-- Список пирамидальных чисел
pyramidalList :: Int -> [Int]
pyramidalList x
    | x == 0 = []
    | otherwise = pyramidalList (x - 1) ++ [sum (triangleList x)]
    
main :: IO ()
main = do
    print(triangleList 5)
    print(pyramidalList 5)