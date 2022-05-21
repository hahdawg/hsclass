
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

envTriple = (*3)
rdrTriple = Reader (envTriple)
tripleMe = runReader $ fmap id rdrTriple


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


type Config = String

cfg2float :: Config -> Float
cfg2float cfg = read cfg :: Float

computePrice :: Reader Config Float
computePrice = do
    cfg <- ask
    Reader cfg2float
