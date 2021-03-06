-- Опишите функцию, которая для данного числа n создает список из всех попарных сумм чисел от 1 до n. 
-- ( Т.е. [1+1, 1+2, 1+3, ..., 1+n, 2+1, 2+2, ..., n+n] - всего n*n элементов)

add :: Num b => [b] -> [b]
add alist = map (+ 1) alist

fun :: Int -> Int -> [Int]
fun _ 0 = []
fun n k = add ([1..n] ++ fun n (k-1))

main :: IO ()
main = do
    let n = 3
    print(fun n n)