import Control.Applicative
import Control.Monad
import Data.Either

type Name = String
type Age  = Int

data PersonError = NegativeAge 
           | AgeOverMaximum
           | NameTooLong
           | NameEmpty 

instance Show PersonError where
    show AgeOverMaximum = "Your age is too high."
    show NegativeAge    = "You can't use negative age."
    show NameTooLong    = "Your name is too long."
    show NameEmpty      = "Your name can't be empty."

data Person = Person Name Age deriving (Show)

max_Age = 100
max_Len = 10

validateLength :: Int -> Name -> Either PersonError Name
validateLength maxLen name = 
     if length name > maxLen then Left NameTooLong
     else Right name

notEmpty :: Name -> Either PersonError Name
notEmpty s = 
    if length s < 0 then Left NameEmpty
    else Right s
negativeAge :: Age -> Either PersonError Age
negativeAge age = 
    if age < 0 then Left NegativeAge
    else  Right age

maximumAge :: Age -> Age -> Either PersonError Age
maximumAge maxAge age = 
    if age > maxAge then Left AgeOverMaximum
    else Right age

ageFun = [negativeAge, maximumAge max_Age]
nameFun = [notEmpty, validateLength max_Len]

collectErrors :: [Either PersonError a] -> Either [PersonError] a
collectErrors xs = let left = lefts xs
                       right = rights xs
               in  if length left == 0 then Right $ right !! 0
                   else Left left

mkAge :: Age -> Either [PersonError] Age
mkAge age = collectErrors $ ageFun <*> [age] 

mkName :: Name -> Either [PersonError] Name
mkName name = collectErrors $ nameFun <*> [name]

mkPerson :: Name -> Age -> Either [PersonError] Person
mkPerson name age = Person <$> mkName name <*> mkAge age
