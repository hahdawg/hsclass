
type Name = String
type Age = Int
data Person = Person Name Age deriving Show
type ValidatePerson a = Either [PersonInvalid] Person


data PersonInvalid = NameEmpty | AgeNegative deriving (Eq, Show)


makePerson :: Name -> Age -> Either [PersonInvalid] Person
makePerson n a
    | illegalName && illegalAge = Left [NameEmpty, AgeNegative]
    | illegalName = Left [NameEmpty]
    | illegalAge = Left [AgeNegative]
    | otherwise = Right (Person n a)
    where
        illegalName = length n == 0
        illegalAge = a < 0
