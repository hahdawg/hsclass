
-- Custom Writer implementation, because it doesn't exist in new haskell
newtype Writer w a = Writer { runWriter :: (a, w) } deriving (Eq, Show)


instance Functor (Writer w) where
    fmap f (Writer (a, w))  = Writer (f a, w)


instance Monoid w => Applicative (Writer w) where
    pure x = Writer (x, mempty)
    Writer (f, w1) <*> Writer (a, w2) = Writer ((f a), w1 `mappend` w2)


instance Monoid w => Monad (Writer w) where
    return x = Writer (x, mempty)
    Writer (a, w) >>= f = Writer (a', w `mappend` w')
        where
            (a', w') = runWriter (f a)
    Writer (_, w1) >> Writer (x, w2) = Writer(x, w1 `mappend` w2)  -- accumulate logs


-- Simple logging example with do
fLogged :: Double -> Writer [String] Double
fLogged x = Writer (2*x, ["f doubled input!"])

gLogged :: Double -> Writer [String] Double
gLogged x = Writer (0.5*x, ["g halved input!"])

writerChain = pure 20 >>= fLogged >>= gLogged >>= gLogged

writerChainDo =
    do
        x <- pure 20
        y <- fLogged x
        z <- gLogged x
        return z


-- Simple logging example with do
logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Got number: " ++ show x])

runLog :: Writer [String] Int
runLog = do
    Writer(0, ["Doing some multiplication ..."])
    x <- logNumber 1
    y <- logNumber 2
    z <- logNumber 3
    return (x*y*z)


-- Use pointy notation to accumulate logs
writerPointy :: Writer [String] Int
writerPointy = Writer(1, ["a"]) >> Writer(2, ["b"]) >> Writer(3, ["c"])


-- Use bind notation to accumulate logs
increment :: Int -> Writer [String] Int
increment n = Writer(n + 1, ["Got " ++ show n ++ "; Output is " ++ show (n + 1)])

incrementLog :: Writer [String] Int
incrementLog = return 1 >>= increment >>= increment >>= increment >>= increment


-- Log fancyish function with writer
gcd' :: Int -> Int -> Int
gcd' a b
    | b == 0 = a
    | otherwise = gcd' b (a `mod` b)


gcdLogged :: Int -> Int -> Writer [String] Int
gcdLogged a b
    | b == 0 = Writer(a, ["Finished with " ++ show a])
    | otherwise = Writer(a, ["Recursing with (a, b) == " ++ show (a, b)]) 
        >> gcdLogged b (a `mod` b) 
