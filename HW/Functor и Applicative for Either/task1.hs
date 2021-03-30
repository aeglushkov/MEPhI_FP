-- Functor и Applicative для Either

instance Functor (Either a) where
    fmap f (Right b) = Right (f b)
    fmap _ (Left a) = Left a

instance Applicative (Either a) where
    pure = Right
    Left  a <*> _ = Left a
    Right f <*> b = fmap f b
