
import Control.Applicative
import Data.Char


newtype Reader env a = Reader {runReader :: env -> a}

-- fmap :: (a -> b) -> f a -> f b
instance Functor (Reader env) where
    -- fmap :: (a -> b) -> Reader (e -> a) -> Reader (e -> b)
    fmap ab (Reader ea) = Reader $ ab . ea


ask :: Reader a a
ask = Reader (\env -> env)

-- <*> :: f (a -> b) -> f a -> f b
instance Applicative (Reader env) where
    -- pure :: a -> Reader e -> a
    pure x = Reader (\_ -> x)
    -- <*> :: Reader (e -> (a -> b)) -> Reader (e -> a) -> Reader (e -> b)
    Reader eab <*> Reader ea = Reader (\e -> (eab e) (ea e))


-- >>= :: m a -> (a -> m b) -> m b
instance Monad (Reader env) where
    return x = Reader (\_ -> x)
    -- >>= :: Reader (e -> a) -> (a -> Reader (e -> b)) -> Reader (e -> b)
    Reader ea >>= aReb = Reader $ \e -> runReader (aReb . ea $ e) e


-- Example: We have a global constant 5, and we want to call some functions on it
envFive = \_ -> 5
rdrFive = Reader envFive
alwaysSix = runReader $ fmap (+1) rdrFive

envSix = \_ -> 6
rdrSix = Reader envSix
alwaysSeven = runReader $ fmap (+1) rdrSix


tom :: Reader String String
tom = do
    env <- ask
    return (env ++ " This is Tom")


tom' :: Reader String String
tom' = ask >>= (\env -> return (env ++ " This is Tom"))


jerry :: Reader String String
jerry = do
    env <- ask
    return (env ++ " This is Jerry")


tomAndJerry :: Reader String String
tomAndJerry = do
    t <- tom
    j <- jerry
    return (t ++ "\n" ++ j)


runJerryRun :: String
runJerryRun = runReader tomAndJerry "Who is this? "

-- Reader example: t = date
newtype History t a = History { observe :: t -> a }

-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> History (t -> a) -> History (t -> b)
instance Functor (History t) where
    -- apply a function to the context of an historical value
    fmap ab (History ta)  = History (ab . ta)

-- <*> :: f (a -> b) -> f a -> f b
-- fmap :: History (t -> a -> b) -> History (t -> a) -> History (t -> b)
instance Applicative (History t) where
    pure x = History (\_ -> x)
    -- Get the function a -> b at time t and apply it to a at time t
    -- History tab: History of functions a -> b
    -- History ta: History of a
    -- History tb: History of b
    History tab  <*> History ta = History $ \t -> (tab t) (ta t)


-- >>= m a -> (a -> m b) -> m b
-- >>= History (t -> a) -> (a -> History (t -> b)) -> History (t -> b)
instance Monad (History t) where
    return = pure
    -- History ta: History of a values
    -- a -> History tb: Mapping from a to History of b values
    -- History tb: History of b values
    -- e.g. getQBStats = getQB t >>= getPlayerStats t
    History ta >>= aHtb = History $ \t -> observe (aHtb (ta t)) t


-- Example
-- 1. We have a mapping from date to QB name for GB
-- 2. We have a mapping from (QB name, date) to TDs
-- Then bind 1 to 2 to get a mapping  from GB QB to TDs
type Year = Int
type TDS = Int
type QB = String


-- Function to get the name of GB QB at year t
getGBQB :: History Year QB
getGBQB = History (\t -> if t <= 2007 then "Favre" else "Rodgers")


-- Function to get QB statstics 
getTDs :: QB -> History Year TDS
getTDs "Favre" = History (favreTD) where
    favreTD t
        | t <= 2007 = 20 -- Packers stats
        | t <= 2010 = 10 -- Jets/Vikings stats
        | otherwise = 0 -- Retired
getTDs "Rodgers" = History (rodgersTD) where
    rodgersTD t
        | t <= 2007 = 0 -- Not in NFL
        | otherwise = 40 -- Packers stats
getTDs "Manzell" = History (\t -> 0)  -- Not a packer
getTDs "Brady" = History (\t -> 10000)  -- Not a packer
getTDs "Manning" = History (\t -> 30)  -- Not a packer


-- Function to get GB QB statstics
getQBTD :: Year -> Int
getQBTD = observe $ getGBQB >>= getTDs
