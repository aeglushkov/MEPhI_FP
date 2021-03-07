-- Функция delete :: Char -> String -> String, которая принимает на вход строку и символ и возвращает строку, 
-- в которой удалены все вхождения символа. Пример: delete ’l’ "Hello world!" должно возвращать "Heo word!".
delete :: Char -> String -> String
delete i [x] = if x == i then [] else [x]
delete i (x:xs) = if x == i then delete i xs else x : delete i xs

main :: IO ()
main = do
    let i = 'l'
    let list = "Hello world!"
    print(delete i list)