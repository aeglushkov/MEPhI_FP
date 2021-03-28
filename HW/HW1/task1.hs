-- 1)	Разработать тип данных Complex для представления комплексных чисел. Создать селекторы для значений разработанного типа данных realPart и imagPart, которые возвращают действительную и мнимую части комплексного числа соответственно.
--      Complex должен быть экземпляром классов типов Eq и Show.
-- 2)	Создать реализацию класса типов Num для типа данных Complex

data Complex = Complex {
    real :: Float,
    imaginary :: Float
} deriving (Eq, Show)

instance Num Complex where
    (Complex r1 i1) + (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)
    (Complex r1 i1) - (Complex r2 i2) = Complex (r1 - r2) (i1 - i2)
    (Complex r1 i1) * (Complex r2 i2) = Complex (r1 * r2 - i1 * i2) (r1 * i2 + r2 * i1)
    negate (Complex r i) = Complex (-r) (-i)
    abs (Complex r i) = Complex ((r*r + i*i)**(1/2)) 0
    signum (Complex r i) = Complex (r / (r*r + i*i)**(1/2)) (i / (r*r + i*i)**(1/2)) 
