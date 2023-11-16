import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

data Bigger a b = Bigger a b b b deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
 fmap f (Bigger a b1 b2 b3) = Bigger a (f b1) (f b2) (f b3)

instance Foldable (Bigger a) where
 foldMap f (Bigger a b1 b2 b3) = (f b1) <> (f b2) <> (f b3)

instance Traversable (Bigger a) where
 traverse f (Bigger a b1 b2 b3) = (Bigger a) <$> (f b1) <*> (f b2) <*> (f b3)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
 arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
 (=-=) = eq

type TI = Bigger Int

main = do
         let trigger :: TI (Sum Int, Product Int, Int, Sum Int)
             trigger = undefined
         quickBatch (traversable trigger)
