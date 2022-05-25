

-- >>= : m a -> (a -> m b) -> m b
-- >>= :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
-- pure x = (\_ -> x)


maybeEx = do
    x <- Just 1
    y <- Just 2
    z <- Just 3
    Just (x + y + z)


maybeEx' =
    Just 1 >>= \x ->
    Just 2 >>= \y ->
    Just 3 >>= \z ->
    Just (x + y + z)


type Config = Int

inc :: Config -> Int
inc cfg = cfg + 1


branch :: Config -> (Int -> Int)
branch cfg x 
    | cfg < 0 = -1*x
    | otherwise = 10*x


ex :: Config -> Int
ex = inc >>= branch


addStuffDo :: Int -> Int
addStuffDo = do
    a <- (*2)
    b <- (+10)
    return (a + b)


addStuffBind :: Int -> Int
addStuffBind =
    (*2) >>= \a ->
    (+10) >>= \r ->
    return (a + r)


addStuffFn :: Int -> Int
addStuffFn_ra :: Int -> Int
addStuffFn_ra r = 2*r
addStuffFn_arb :: Int -> Int -> Int
addStuffFn_arb a r = a + r + 10
addStuffFn = addStuffFn_ra >>= addStuffFn_arb
