import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

data Pair a b = Pair a b deriving (Eq, Ord, Show)

instance Functor (Pair a) where
 fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
 foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
 traverse f (Pair a b) = Pair <$> pure a <*> (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
 arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
 (=-=) = eq

type TI = Pair Int

main = do
         let trigger :: TI (Sum Int, Product Int, Int, Sum Int)
             trigger = undefined
         quickBatch (traversable trigger)
