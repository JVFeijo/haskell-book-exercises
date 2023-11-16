{-# LANGUAGE InstanceSigs #-}
import Data.Char
import Control.Applicative

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = rev . cap

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) rev cap

tupled' :: [Char] -> ([Char], [Char])
tupled' = (\x -> ((,) (rev x))) >>= (\z x -> (z (cap x)))

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
 fmap f (Reader ra) = Reader $ (\r -> f (ra r))

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Applicative (Reader r) where
 pure :: a -> Reader r a
 pure a = Reader $ (\x -> a)
 
 (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
 (Reader rab) <*> (Reader ra) = Reader $ (\r -> rab r (ra r))

instance Monad (Reader r) where
 return = pure
 
 (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
 (Reader ra) >>= aRb = Reader $ (\r -> runReader (aRb (ra r)) r)


newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person {
  humanName :: HumanName,
  dogName :: DogName,
  address :: Address
} deriving (Eq, Show)

data Dog = Dog {
  dogsName :: DogName,
  dogsAddress :: Address
} deriving (Eq, Show)

getDogRM :: Person -> Dog
getDogRM = runReader $ Dog <$> Reader dogName <*> Reader address

getDogRM' :: Person -> Dog
getDogRM' = runReader $ Reader (Dog . dogName) >>= (<$> (Reader address))
