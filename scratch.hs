


-- >>= :: m a -> (a -> m b) -> m b
-- >>= :: [a] -> (a -> [b]) -> [b]
-- instance Monad [] where
--    return x = [x]
--    xs >>= f = concat $ map f xs


pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = 
    xs >>= \x -> 
    ys >>= \y ->
    g x y
    where
        g x' y' = [(x', y')]


div2 :: Int -> Maybe Int
div2 x 
    | even x = Just (div x 2)
    | otherwise = Nothing


div4 x = return x >>= div2 >>= div2

div4Do x =
    do
        x1 <- div2 x
        x2 <- div2 x1
        return x2
