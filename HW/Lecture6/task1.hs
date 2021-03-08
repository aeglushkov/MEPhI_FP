-- Функция permute, генерация списка всех перестановок списка
-- permute [1,2,3] = [[1,2,3],[1,3,2],…,[3,2,1]]
permute' :: Eq a => [a] -> [[a]]
permute' [] = [[]]
permute' list = let
  listOfListsWith a = map (\e -> a : e) (permute'
    (filter (\x -> x /= a) list))
  in concat $ map listOfListsWith list

-- Функция concat, конкатенация списка списков
-- concat [[1,2],[3,4,5],[6]] = [1,2,3,4,5,6]
concat1 :: [[a]] -> [a]
concat1 = foldr (++) []

concat2 :: [[x]] -> [x]
concat2 [[x]] = [x]
concat2 (x:xs) = x ++ concat2 xs

main :: IO ()
main = do
    print(permute' [1, 2, 3])
    print(permute' [1, 2, 1])

    print(concat1 [[1,2], [3,4,5], [6]])
    print(concat2 [[1,2], [3,4,5], [6]])
