import Data.List

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah


mySwap :: (a, b) -> (b, a)
mySwap (x, y) = (y, x)

x :: Num a => a
x = 3

mySort :: [Char] -> [Char]
mySort = sort


chk :: Eq b => (a -> b) -> a -> b
chk f x = f x


arith :: Num b => (a -> b) -> Integer -> a -> b
arith f n x = sum([(f x) | _ <- [1..n]])


data TisAnInteger = Tisan Integer
instance Eq TisAnInteger where
    Tisan x == Tisan y = (x == y)


data TwoInts = Two Int Int
instance Eq TwoInts where
    Two x1 x2 == Two y1 y2 = (x1 == y1) && (x2 == y2)


data StringOrInt = TisInt Int | TisString String
instance Eq StringOrInt where
    TisInt x == TisInt y = x == y
    TisString x == TisString y = x == y


data Pair a = Pair a a
instance (Eq a) => Eq (Pair a) where
    Pair x1 x2 == Pair y1 y2 = (x1 == x2) && (y1 == y2)
    

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    Tuple x1 x2 == Tuple y1 y2 = (x1 == y1) && (x2 == y2)


data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    Hello x == Hello y = (x == y)
    Goodbye x == Goodbye y = (x == y)


newtype UserName = UserName String
newtype AccountNo = AccountNo Integer
data User = UnregisteredUser | RegisteredUser UserName AccountNo

instance Show User where
    show UnregisteredUser = "UnregisteredUser"
    show (RegisteredUser (UserName name) (AccountNo n)) = name ++ " " ++ (show n)


data WherePenguinsLive =
    Galapagos
    | Antarctica
    | Australia
    | SouthAfrica
    deriving (Show, Eq)


data Penguin = Peng WherePenguinsLive deriving (Show, Eq)

isSouthAfrica :: Penguin -> Bool
isSouthAfrica (Peng Australia) = True
isSouthAfrica _ = False


gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng x) = x


wtf :: (a, b) -> (c, d) -> ((b, d), (a, c))
wtf (a, b) (c, d) = ((b, d), (a, c))
