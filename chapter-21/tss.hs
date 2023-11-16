import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

data S n a = S (n a) a deriving (Eq, Ord, Show)

instance Functor n => Functor (S n) where
 fmap f (S na1 a2) = S (f <$> na1) (f a2)

instance Foldable n => Foldable (S n) where
 foldMap f (S na1 a2) = foldMap ((<>(f a2)) . f) na1

instance Traversable n => Traversable (S n) where
 traverse f (S na1 a2) = S <$> (sequenceA (f <$> na1)) <*> (f a2)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
 arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) => EqProp (S n a) where
 (=-=) = eq

type TI = S []

main = do
         let trigger :: TI (Sum Int, Product Int, Int, Sum Int)
             trigger = undefined
         quickBatch (traversable trigger)
