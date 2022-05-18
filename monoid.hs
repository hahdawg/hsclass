import Control.Monad
import Data.Monoid
import Test.QuickCheck


data Optional a = Nada | Only a deriving (Eq, Show)


instance (Semigroup a) => Semigroup (Optional a) where
    Nada <> _ = Nada
    _ <> Nada = Nada
    (Only x) <> (Only y) = Only (x <> y)


instance (Monoid a) => Monoid (Optional a) where
    mempty = Nada
    mappend Nada _ = Nada
    mappend _ Nada = Nada
    mappend (Only x) (Only y) = Only (mappend x y)


newtype First' a = First' { getFirst' :: Optional a} deriving (Eq, Show)


instance Semigroup a => Semigroup (First' a) where
    First' Nada <> _ = First' Nada
    First' _ <> First' Nada = First' Nada
    First' (Only x) <> First' (Only y) = First' (Only x <> Only y)


instance Semigroup a => Monoid (First' a) where
    mempty = First' Nada


newtype NonEmpty' a = NonEmpty'' (a, [a]) deriving (Eq, Ord, Show)


instance Semigroup (NonEmpty' a) where
    NonEmpty'' (x, [xs]) <> NonEmpty'' (y, [ys]) = NonEmpty'' (x, [xs] ++ [ys])


data Trivial = Trivial deriving (Eq, Show)


instance Semigroup Trivial where
    Trivial <> Trivial = Trivial


instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)


newtype Identity a = Identity a deriving (Eq, Show)


instance Semigroup a => Semigroup (Identity a) where
    Identity x <> Identity y = Identity (x <> y)


instance Monoid a => Monoid (Identity a) where
    mempty = Identity (mempty)
    mappend = (<>)


data Two a b = Two a b


instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)


instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)


newtype BoolConj = BoolConj Bool


instance Semigroup BoolConj where
    BoolConj True <> BoolConj True = BoolConj True
    BoolConj _ <> BoolConj _ = BoolConj False


instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)


data Or a b = Fst a | Snd b deriving (Eq, Show)


instance Semigroup (Or a b) where
    Fst _ <> Snd x = Snd x
    Snd x <> Fst _ = Snd x
    Fst _ <> Fst x = Fst x
    Snd _ <> Snd x = Snd x


instance (Monoid a, Monoid b) => Monoid (Or a b) where
    mempty = Fst mempty
    mappend = (<>)


newtype Combine a b = Combine {unCombine :: (a -> b)}


instance Semigroup b => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine (f <> g)


instance Monoid b => Monoid (Combine a b) where
    mempty = Combine (\x -> mempty)
    mappend = (<>)


newtype Mem s a = Mem { runMem :: s -> (a, s) }


instance Semigroup a => Semigroup (Mem s a) where
     Mem f <> Mem g = Mem h
        where
            h s = (af <> ag, s)
                where
                    (af, _) = f s
                    (ag, _) = g s


instance Monoid a => Monoid (Mem s a) where
    mempty = Mem (\s -> (mempty, s))
    mappend = (<>)
