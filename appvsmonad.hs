
import Control.Monad (join)

data Cow = Cow { name :: String, age :: Int, weight :: Int}


noEmpty :: String -> Maybe String
noEmpty x
    | length x == 0 = Nothing
    | otherwise = Just x


noNegative :: Int -> Maybe Int
noNegative n
    | n < 0 = Nothing
    | otherwise = Just n


weightCheck :: Cow -> Maybe Cow
weightCheck (Cow n a w)
    | n == "Bess" && w > 499 = Nothing
    | otherwise = Just (Cow n a w)


-- case example
mkCowCase :: String -> Int -> Int -> Maybe Cow
mkCowCase n a w =
    case noEmpty n of
        Nothing -> Nothing
        Just n' -> case noNegative a of
            Nothing -> Nothing
            Just a' -> case noNegative w of
                Nothing -> Nothing
                Just w' -> weightCheck (Cow n' a' w')


-- instance Applicative Maybe where
--    Nothing <*> _ = Nothing
--    _ <*> Nothing = Nothing
--    Just f <*> x  f x


-- NOTE: Need Monad to shove (Maybe Cow) into weightCheck
mkCowApp :: String -> Int -> Int -> Maybe Cow
mkCowApp n a w = Cow <$> noEmpty n <*> noNegative a <*> noNegative w >>= weightCheck
-- Cow <$> noEmpty n  ~  Cow n a w <$> Maybe n  ~  \a w -> Maybe (Cow N a w)
-- (\a w -> Maybe (Cow N a w)) <*> Maybe a  ~  \w -> Maybe (Cow N A w) 
-- (\w -> Maybe (Cow N A w)) <*> Maybe w  ~  Maybe (Cow N A W)


mkCowMonadDo :: String -> Int -> Int -> Maybe Cow
mkCowMonadDo n a w = do
    n' <- noEmpty n
    w' <- noNegative w
    a' <- noNegative a
    weightCheck (Cow n' w' a')


-- instance Monad Maybe where
--     Nothing >>= _ = Nothing
--     Just x >> f = f x


-- Note: If Nothing gets shoved into any >>=, then whole chain returns Nothing
mkCowMonadBind :: String -> Int -> Int -> Maybe Cow
mkCowMonadBind n a w =
    noEmpty n >>= \n' ->
    noNegative a >>= \a' ->
    noNegative w >>= \w' ->
    weightCheck (Cow n' w' a')  -- Note: If we got this far, we know that n' w' and a' are not Nothing


-- StackOverflow example
checkPwdM :: IO ()
checkPwdM = do
    putStrLn "What's the password?"
    pass <- getLine
    if pass == "swordfish"
        then putStrLn "Correct!"
        else putStrLn "Wrong!"


if' :: Bool -> a -> a -> a
if' True t _ = t
if' False _ f = f


checkPwdA :: IO ()
checkPwdA = do
    putStrLn "What's the password?"
    pass <- getLine
    right <- putStrLn "Correct!" -- This runs regardless
    wrong <- putStrLn "Wrong"  -- This runs regardless
    pure (if' (pass == "swordfish") right wrong)


checkPwdFn :: String -> IO ()
checkPwdFn pass = if pass == "swordfish"
    then putStrLn "Correct!"
    else putStrLn "Wrong!"


checkPwdA' :: IO (IO ())
checkPwdA' = checkPwdFn <$> (putStrLn "What's the password?" *> getLine)


checkPwdM' :: IO ()
checkPwdM' = join checkPwdA'


data Logger a = Logger String a


-- StackOverflow example
ifM' :: Monad m => m Bool -> m a -> m a -> m a
ifM' c x y = c >>= \b -> if b then x else y

ifA' :: Applicative f => f Bool -> f a -> f a -> f a
ifA' c x y = f <$> c <*> x <*> y
    where
        f = \c' x' y' -> if c' then x' else y'
-- NOTE: ifA' works fine with any Just values, but it returns Nothing if x or y is Nothing, regardless of what we pass for c
-- Here, if f = Maybe, c = Just True, x = Just 1, and y = Nothing, we end up with Maybe f <*> Nothing = Nothing
-- 1. f c x y <$> Just True <*> Just 1 <*> Nothing
-- 2. Just (f True x y) <*> Just 1 <*> Nothing
-- 3. Just (f True 1 y) <*> Nothing = Nothing
