import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

data Big a b = Big a b b deriving (Eq, Ord, Show)

instance Functor (Big a) where
 fmap f (Big a b1 b2) = Big a (f b1) (f b2)

instance Foldable (Big a) where
 foldMap f (Big a b1 b2) = (f b1) <> (f b2)

instance Traversable (Big a) where
 traverse f (Big a b1 b2) = (Big a) <$> (f b1) <*> (f b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
 arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
 (=-=) = eq

type TI = Big Int

main = do
         let trigger :: TI (Sum Int, Product Int, Int, Sum Int)
             trigger = undefined
         quickBatch (traversable trigger)
