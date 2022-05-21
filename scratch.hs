import Control.Monad
import Data.Char

bind :: (Monad m) => m a -> (a -> m b) -> m b
bind x f = join $ fmap f x


binding :: IO ()
binding = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn ("Your name is " ++ name)


binding' = 
    putStrLn "What's your name?" >>
    getLine >>= 
    (\s -> putStrLn ("Your name is " ++ s))


twoBinds :: IO ()
twoBinds = do
    putStrLn "name pls:"
    name <- getLine
    let nameUpper = map toUpper name  -- let binding
    putStrLn "age pls:"
    age <- getLine
    putStrLn ("Your name is " ++ nameUpper ++ ", and you are " ++ age ++ " years old")


twoBinds' :: IO ()
twoBinds' =
    putStrLn "name pls:" >> 
    getLine >>= \name ->
    putStrLn "age pls:" >>
    getLine >>= \age ->
    putStrLn ("Your name is " ++ map toUpper name ++ ", and you are " ++ age ++ " years old")


twiceWhenEven :: [Int] -> [Int]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x, x]
        else [x]


twiceWhenEven' :: [Int] -> [Int]
twiceWhenEven' xs =
    xs >>= twe
    where
        twe x
            | even x = [x, x]
            | otherwise = [x]


untilNull :: IO ()
untilNull = do
    putStrLn ("Enter some text")
    line <- getLine
    if null line
        then return ()
        else (do
            putStrLn $ reverse line
            untilNull
        )


processLine :: String -> IO (String)
processLine s
    | null s = return "" 
    | otherwise = return (reverse s)



untilNull' :: IO ()
untilNull' =
    putStrLn ("Enter some text") >>
    getLine >>= processLine >>= (\proc -> if null proc then return () else untilNull')
