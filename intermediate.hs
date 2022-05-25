

class Fluffy f where
    furry :: (a -> b) -> f a -> f b


instance Fluffy [] where
    furry f (x:xs) = f x : furry f xs


instance Fluffy Maybe where
    furry f Nothing = Nothing
    furry f (Just x) = Just (f x)


instance Fluffy ((->) t) where
    furry f g = f.g


newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)


instance Fluffy (EitherLeft b) where
    furry f (EitherLeft (Left x)) = EitherLeft (Left (f x))
    furry f (EitherLeft (Right x)) = EitherLeft (Right x)


class Misty m where
    banana :: (a -> m b) -> m a -> m b
    unicorn :: a -> m a
    furry' :: (a -> b) -> m a -> m b
    furry' f ma = banana (\x -> unicorn $ f x) ma


instance Misty [] where
    unicorn x = [x]
    banana f (x:xs) = f x ++ banana f xs


instance Misty Maybe where
    unicorn x = Just x
    banana _ Nothing = Nothing
    banana f (Just x) = f x


instance Misty ((->) t) where
    unicorn x = (\_ -> x)
    -- banana (a -> (t -> b)) -> (t -> a) -> (t -> b)
    banana atb ta = \t -> atb (ta t) t


jellybean :: (Misty m) => m (m a) -> m a
jellybean x = banana id x
