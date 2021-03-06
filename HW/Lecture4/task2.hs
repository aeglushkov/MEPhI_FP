-- Функция substitute :: Char -> Char -> String -> String, которая заменяет в строке указанный символ на заданный. 
-- Пример: substitute ’e’ ’i’ "eigenvalue" возвращает "iiginvalui"

substitute :: Char -> Char -> String -> String
substitute i j [x] = if head [x] == i then [j] else [x]
substitute i j (x:xs) = if x == i then j : substitute i j xs else x : substitute i j xs

main :: IO ()
main = do
    let i = 'e'
    let j = 'i'
    let list = "eigenvalue"
    print(substitute i j list)