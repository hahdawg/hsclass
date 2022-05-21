
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


-- do notation
mkCow :: String -> Int -> Int -> Maybe Cow
mkCow name age weight = do
    name' <- noEmpty name
    age' <- noNeg age
    weight' <- noNeg weight
    weightCheck (Cow name' age' weight')


-- bind notation
mkCow' :: String -> Int -> Int -> Maybe Cow
mkCow' name age weight =
    noEmpty name >>= \n ->
    noNeg age >>= \a ->
    noNeg weight >>= \w ->
    weightCheck (Cow name age weight)


-- StackOverflow example
fireMissles :: IO (String)
fireMissles = do
    pure ("Missles Fired") 


doNothing :: IO (String)
doNothing = do
    pure ("Nothing done") 


demoM :: IO (String)
demoM = do
    b <- readLn :: IO Bool
    if b
        then fireMissles
        else doNothing


demoA :: IO (IO (String))  -- Nested IO, so can't use without join
demoA = (\b -> if b then fireMissles else doNothing) <$> readLn


demoA' :: IO (IO (String))
demoA' = pure f <*> readLn
    where f = \b -> if b then fireMissles else doNothing


ifM' :: Monad m => m Bool -> m a -> m a -> m a
ifM' c x y = c >>= \z -> if z then x else y


ifA :: Applicative f => f Bool -> f a -> f a -> f a
ifA c x y = (\c' x' y' -> if c' then x' else y') <$> c <*> x <*> y


type Founded = Int
type Coders = Int

data SoftwareShop = Shop { founded :: Founded, programmers :: Coders } deriving (Eq, Show)


data FoundedError = 
    NegativeYears Founded
    | TooManyYears Founded
    | NegativeCoders Coders
    | TooManyCoders Coders
    | TooManyCodersForYears Founded Coders
    deriving (Eq, Show)

validateFounded n
    | n < 0 = Left $ NegativeYears n
    | n > 500 = Left $ TooManyYears n
    | otherwise = Right n

validateCoders n
    | n < 0 = Left $ NegativeCoders n
    | n > 5000 = Left $ TooManyYears n
    | otherwise = Right n


mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    founded <- validateFounded years
    programmers <- validateCoders coders
    if programmers > div founded 10
        then Left $ TooManyCodersForYears founded programmers
        else Right $ Shop founded programmers


mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g x = g x >>= f


-- You could've invented Monads

-- Basic Fns
f :: Float -> Float
f x = 2 * x

g :: Float -> Float
g x = 0.5 * x


-- Logged basic functions
f' :: Float -> (Float, String)
f' x = (f x, "f ran")


g' :: Float -> (Float, String)
g' x = (g x, "g ran")


-- bnd: pass logged output to logged function
bnd :: (Float, String) -> (Float -> (Float, String)) -> (Float, String)
bnd (x, s) f = (y, s ++ "; " ++ s')
    where
        (y, s') = f x


unit :: Float -> (Float, String)
unit x = (x, "")


-- imitation bind operator
(>>?) :: (Float, String) -> (Float -> (Float, String)) -> (Float, String)
(x, s) >>? f = bnd (x, s) f

chain = unit 100.0 >>? f' >>? f' >>? g' >>? g'
