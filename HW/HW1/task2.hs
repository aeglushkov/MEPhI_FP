-- Определить экземпляры классов типов Functor и Applicative для типа данных Fun.

module Fun where
  newtype Fun a b = Fun {getFun :: a -> b}

  instance Functor (Fun a) where
    fmap f (Fun a) = Fun (f . a)

  instance Applicative (Fun a) where
    pure a = Fun (\_ -> a)
    Fun a <*> Fun b = Fun (\x -> (a x) (b x)) 
