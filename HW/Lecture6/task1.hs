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
concat' :: [[a]] -> [a]
concat' = foldr (++) []

main :: IO ()
main = do
    print(permute' [1, 2, 3])
    print(permute' [1, 2, 1])
    print(concat' [[1,2], [3,4,5], [6]])