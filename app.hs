


data Pair a b = Pair a b deriving (Show, Eq)


instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
    Pair x1 y1 <> Pair x2 y2 = Pair (x1 <> x2) (y1 <> y2)


instance (Monoid a, Monoid b) => Monoid (Pair a b) where
    mempty = Pair mempty mempty


instance Functor (Pair a) where
    fmap f (Pair x y) = Pair x (f y)


instance Monoid a => Applicative (Pair a) where
    pure x = Pair mempty x
    (Pair x f) <*> (Pair y z) = Pair (x `mappend` y) (f z)


newtype Identity a = Identity a deriving (Eq, Ord, Show)


instance Functor Identity where
    fmap f (Identity x) = Identity (f x)


instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity (f x)


newtype Constant a b = Constant' { getConstant :: a } deriving (Eq, Ord, Show)


instance Functor (Constant a) where
    fmap f (Constant' x) = Constant' x 


instance Monoid a => Applicative (Constant a) where
    pure x = Constant' mempty
    (Constant' x) <*> (Constant' y) = Constant' (x <> y)


validateLength :: Int -> String -> Maybe String
validateLength maxLen s
    | length s > maxLen = Nothing
    | otherwise = Just s


newtype Name = Name String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a


data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
    case mkName n of
        Nothing -> Nothing
        Just n' ->
            case mkAddress a of
                Nothing -> Nothing
                Just a' -> Just (Person n' a')


mkPerson' n a = Person <$> (mkName n) <*> (mkAddress a)


data CouldBe a = Nada | Solo a


instance Functor CouldBe where
    fmap _ Nada = Nada
    fmap f (Solo x) = Solo (f x)


instance Applicative CouldBe where
    pure = Solo
    Nada <*> _ = Nada
    _ <*> Nada = Nada
    Solo f <*> Solo x = Solo (f x)


data Cow = Cow {name :: String, age :: Int, weight :: Int} deriving (Eq, Show)


noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty x = Just x


noNegative :: Int -> Maybe Int
noNegative n
    | n < 0 = Nothing
    | otherwise = Just n


mkCow :: String -> Int -> Int -> Maybe Cow
mkCow n a w = Cow <$> noEmpty n <*> noNegative a <*> noNegative w


data List a = Nil | Cons a (List a) deriving (Eq, Show)


instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
    

append :: List a -> List a -> List a
append xs Nil = xs
append Nil xs = xs
append (Cons x xs) ys = Cons x (append xs ys)


instance Applicative List where
    pure x = Cons x Nil
    _ <*> Nil = Nil
    Nil <*> _ = Nil
    (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)


newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)


instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs


zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' _ Nil _ = Nil
zipWith' _ _ Nil = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)


repeat' :: a -> List a
repeat' x = Cons x (repeat' x)


instance Applicative ZipList' where
    pure x = ZipList' (repeat' x)
    ZipList' fs <*> ZipList' xs  = ZipList' (zipWith' ($) fs xs)


data Either' a b = Left' a | Right' b


instance Functor (Either' a) where
    fmap f (Left' x) = Left' x
    fmap f (Right' x) = Right' (f x)


instance Applicative (Either' a) where
    pure x = Right' x
    (Right' f) <*> (Right' x) = Right' (f x)
    (Left' x) <*> _ = Left' x
    _ <*> (Left' x) = Left' x



f x = lookup x [(3, "hi"), (4, "yo"), (5, "kbai")]
g x = lookup x [(7, "dutch"), (8, "sunny"), (9, "gramma")]

greetName :: Int -> Int -> Maybe String
greetName x y = (++) <$> f x <*> g y
