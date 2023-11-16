import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

data List a = Nil | Cons a (List a)
 deriving (Eq, Ord, Show)

instance Functor List where
 fmap f Nil = Nil
 fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
 foldMap f Nil = mempty
 foldMap f (Cons x xs) = (f x) <> foldMap f xs

instance Traversable List where
 traverse f Nil = pure Nil
 traverse f (Cons x xs) = Cons <$> (f x) <*> (traverse f xs)

instance Arbitrary a => Arbitrary (List a) where
 arbitrary = frequency [(1, pure Nil), (5, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
 (=-=) = eq

type TI = List

main = do
         let trigger :: TI (Sum Int, Product Int, Int, Sum Int)
             trigger = undefined
         quickBatch (traversable trigger)
