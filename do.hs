

cartDo :: [a] -> [b] -> [(a, b)]
cartDo xs ys =
    do 
        x <- xs
        y <- ys
        [(x, y)]


cartBind :: [a] -> [b] -> [(a, b)]
cartBind xs ys = 
    xs >>= \x -> ys >>= \y -> [(x, y)]


just6M = Just 5 >>= (\x -> Just (x + 1))
just6F = fmap (+1) (Just 5)


doubleEvenDo :: [Int] -> [Int]
doubleEvenDo xs =
    do
        x <- xs
        if even x
            then [x, x]
        else
            [x]


doubleEvenBind :: [Int] -> [Int]
doubleEvenBind xs =
    xs >>= \x -> if even x then [x, x] else [x]
