

data MyList a = MyEmpty | Cons a (MyList a) deriving Show


data Vector a = VectorCons a a a 


data Rand a b c = RandCons a b c | Pair a b | Single c


data Manufacturer = Honda | BMW deriving (Eq, Show)
data Price = Price Int deriving (Eq, Show)
data Airline = United | TWA deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _)  = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> Bool
areCars [Car _ _] = True
areCars _ = False


getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _ = error "Only cars have manufacts!"


type Fish = Int

mySum :: Int -> Int -> Int
mySum x y = x + y


class TooMany a where
    tooMany :: a -> Bool


instance TooMany Int where
    tooMany x = x > 42


instance TooMany Integer where
    tooMany x = x > 42


instance TooMany Char where
    tooMany x = False


data IntString = IntString Int String


data IntInt = IntInt Int Int


instance TooMany IntString where
    tooMany (IntString x _) = tooMany (x::Int)


instance TooMany IntInt where
    tooMany (IntInt x y) = tooMany (x + y)


data BigSmall = Big Bool | Small Bool deriving (Eq, Show, Ord)


data Person = Person {name :: String, age :: Int} deriving (Eq, Show)


data Fiction = Fiction' deriving Show
data Nonfiction = Nonfiction' deriving Show
data BookType = FictionBook Fiction | NonfictionBook Nonfiction

type AuthorName = String
-- data Author = Author (AuthorName, BookType)

data Author = 
    Fiction AuthorName | Nonfiction AuthorName
    deriving Show


data Auto = Null | MyCar {make :: String, model :: String}
