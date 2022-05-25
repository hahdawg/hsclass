
import System.Random


popDumb :: (Int, [Int]) -> (Int, [Int])
popDumb (_, (x:xs)) = (x, xs)


pushDumb :: (Int, [Int]) -> (Int, [Int])
pushDumb (x, xs) = (x, x:xs)

stackDumb = 
    let
        (_, stack1) = pushDumb (1, [])
        (_, stack2) = pushDumb (2, stack1)
        (_, stack3) = pushDumb (3, stack2)
        (_, stack4) = pushDumb (4, stack3)
        (x, stack5) = popDumb (0, stack4)
    in pushDumb (5, stack5)


pushFn :: Int -> [Int] -> (Int, [Int])
pushFn x = \xs -> (x, x:xs)


popFn :: [Int] -> (Int, [Int])
popFn = \(x:xs) -> (x, xs)


stackFn =
    let
        (_, stack1) = pushFn 1 []
        (_, stack2) = pushFn 2 stack1
        (_, stack3) = pushFn 3 stack2
        (_, stack4) = pushFn 4 stack3
        (x, stack5) = popFn stack4
    in pushFn 5 stack5


newtype State s a = State { runState :: s -> (a, s) }

-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> State (s -> (a, s)) -> State (s -> (b, s))
-- s -> [st] -> (x, s') =={fmap}=> s -> [st] -> (f x, s')
instance Functor (State s) where
    fmap f (State g) = State $ \stack ->
        let
            (x, newStack) = g stack
        in
            (f x, newStack)

-- <*> :: f (a -> b) -> f a -> f b
-- <*> :: State (s -> (a -> b, s)) -> State (s -> (a, s)) -> State (s -> (b, s))
instance Applicative (State s) where
    pure x = State $ (\stack -> (x, stack))
    State f <*> State g = State $ \stack ->
        let
            (a, stackG) = g stack
            (f', stackF) = f stack
        in
            (f' a, stackF)


-- >>= :: m a -> (a -> m b) -> m b
-- >>= State (s -> (a, s)) -> (a -> State (s -> (b, s))) -> State (s -> (b, s))
-- Bind mainly composes state transition functions
instance Monad (State s) where
    return = pure
    State f >>= g = State $ \stack ->
        let
            (a, stack') = f stack  -- pull f out of State, and evaluate it on s0, to get (a, s1)
            (State h) = g a  -- evaluate g on value output of f
            (b, stack'') = h stack'  -- evaluate h on s1 to get (b, s2)
        in
            (b, stack'')
            
            
push :: Int -> State [Int] Int
push x = State (\xs -> (x, x:xs))


pop :: State [Int] Int
pop = State (\(x:xs) -> (x, xs))


-- Note the pop >> x
-- The state output of 
--  push 4 = (4, [4, 3, 2, 1) = (a, s)
--  but only 'a' gets bound in \x -> push (x + 100)
stackBind :: State [Int] Int
stackBind =           -- (_,    [])
    push 1 >>= \_ ->  -- (1,    [1])
    push 2 >>= \_ ->  -- (2,    [2, 1])
    push 3 >>= \_ ->  -- (3,    [3, 2, 1])
    push 4 >>= \_ ->  -- (4,    [4, 3, 2, 1])
    pop >>= \x ->     -- (4,    [3, 2, 1])
    push (x + 100)    -- (104,  [104, 3, 2, 1])


stackDo :: State [Int] Int
stackDo = do
    push 1
    push 2
    push 3
    push 4
    x <- pop
    push (x + 100)


-- random :: (RandomGen g, Uniform a) => g -> (a, g)
-- to use: runState randomSt (mkStdGen 3)
randomSt :: (RandomGen g, Random a) => State g a
randomSt = State random


threeCoinsDo :: State StdGen (Bool, Bool, Bool)
threeCoinsDo = do
    x <- randomSt
    y <- randomSt
    z <- randomSt
    return (x, y, z)


threeCoinsBind :: State StdGen (Bool, Bool, Bool)
threeCoinsBind = 
    randomSt >>= \x -> 
    randomSt >>= \y ->
    randomSt >>= \z ->
    return (x, y, z)


maybeBind :: Maybe Int
maybeBind =
    Just 1 >>= \x ->
    Just 2 >>= \y -> 
    return (x + y)


maybeDo :: Maybe Int
maybeDo = do
    x <- Just 1
    y <- Just 2
    return (x + y)


ioDo :: IO ()
ioDo = do
    x <- getLine
    y <- getLine
    putStrLn("x was " ++ show x ++ "; y was " ++ show y)


ioBind :: IO ()
ioBind = 
    getLine >>= \x ->
    getLine >>= \y ->
    putStrLn("x was " ++ show x ++ "; y was " ++ show y)
