


class Functor' f where
    fmap' :: (a -> b) -> f a -> f b


data Dutch a = Dutch' a deriving Show


class Sumthin a where
    sumthin :: a -> a


instance Sumthin (Dutch a) where
    sumthin (Dutch' x) = Dutch' x


data FixMePls a = FixMe | Pls a deriving (Eq, Show)


instance Functor (FixMePls) where
    fmap _ FixMe = FixMe
    fmap f (Pls x) = Pls (f x)


replaceWithP = const 'p'


data Two a b = Two a b deriving (Eq, Show)


instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)


data Three a b c = Three a b c


instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)


data Three' a b = Three' a b b


instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)


liftedInc :: (Functor f, Num a) => f a -> f a
liftedInc = fmap (+1)


data Possibly a = LolNope | Yeppers a deriving (Eq, Show)


instance Functor Possibly where
    fmap f LolNope = LolNope
    fmap f (Yeppers x) = Yeppers (f x)


incIfRight :: Num b => Either a b -> Either a b
incIfRight (Left x) = Left x
incIfRight (Right x) = Right $ x + 1


newtype Constant' a b = Constant' { getConstant' :: a} deriving (Eq, Show)


instance Functor (Constant' a) where
    fmap f (Constant' x) = Constant' x


data Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
    fmap g (Wrap x) = Wrap (fmap g x)
