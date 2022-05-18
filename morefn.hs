

myflip :: (a -> b -> c) -> b -> a -> c
myflip f b a = f a b


returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f a c = f a


data MyList a = MyEmpty | Cons a (MyList a)
