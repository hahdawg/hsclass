
import Control.Monad

fmap' f xs = xs >> return . f


bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x


sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "another blah"


sequencing' :: IO ()
sequencing' =
    putStrLn "blah" >>
    putStrLn "another blah"


binding :: IO ()
binding = do
    name <- getLine
    putStrLn name


binding' :: IO ()
binding' = getLine >>= putStrLn


bindAndSeq :: IO ()
bindAndSeq = do
    putStrLn "name pls"
    name <- getLine
    putStrLn ("hello " ++ name)


bindAndSeq' :: IO ()
bindAndSeq' =
    putStrLn "name pls" >>
    getLine >>= (\s -> putStrLn ("hello " ++ s))


twoBinds :: IO ()
twoBinds = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn "age pls:"
    age <- getLine
    putStrLn ("hello " ++ name ++ "; you're " ++ age ++ "years old!")


twoBinds' =
    putStrLn "name pls" >>
    getLine >>= \name ->
    putStrLn "age pls" >>
    getLine >>= \age ->
    putStrLn ("hello " ++ name ++ "; you're " ++ age ++ "years old!")


twiceWhenEven :: [Int] -> [Int]
twiceWhenEven xs = do
    x <- xs
    if even x then [x, x] else []


twiceWhenEven' :: [Int] -> [Int]
twiceWhenEven' xs = xs >>= twe
    where
        twe y
            | even y = [y, y]
            | otherwise = []


data Cow = Cow { name :: String, age :: Int, weight :: Int} deriving (Eq, Show)


noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty x = Just x


noNeg :: Int -> Maybe Int
noNeg x
    | x <= 0 = Nothing
    | otherwise = Just x


weightCheck :: Cow -> Maybe Cow
weightCheck c =
    if n == "Bess" && w > 499
        then Nothing
        else Just c
        where
            n = name c
            w = weight c


mkCow :: String -> Int -> Int -> Maybe Cow
mkCow name age weight = do
    name' <- noEmpty name
    age' <- noNeg age
    weight' <- noNeg weight
    weightCheck (Cow name' age' weight')


mkCow' :: String -> Int -> Int -> Maybe Cow
mkCow' name age weight =
    noEmpty name >>= \n ->
    noNeg age >>= \a ->
    noNeg weight >>= \w ->
    weightCheck (Cow name age weight)


doSomething :: IO (String, String)
doSomething = do
    a <- getLine
    b <- getLine
    pure (a, b)


doSomething' :: IO (String, String)
doSomething' = 
    getLine >>= \a ->
    getLine >>= \b ->
    pure (a, b)


fireMissles :: IO (String)
fireMissles = do
    pure ("Missles Fired") 


doNothing :: IO (String)
doNothing = do
    pure ("Nothing done") 


demo_monad :: IO (String)
demo_monad = do
    b <- readLn :: IO Bool
    if b
        then fireMissles
        else doNothing


demo_fmap = (\b -> if b then fireMissles else doNothing) <$> readLn


demo_applicative = pure f <*> readLn
    where f = \b -> if b then fireMissles else doNothing


ifM' :: Monad m => m Bool -> m a -> m a -> m a
ifM' c x y = c >>= \z -> if z then x else y


ifA :: Applicative f => f Bool -> f a -> f a -> f a
ifA c x y = (\c' x' y' -> if c' then x' else y') <$> c <*> x <*> y
