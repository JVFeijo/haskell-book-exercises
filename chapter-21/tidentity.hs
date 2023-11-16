import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor (Identity) where
 fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
 foldMap f (Identity a) = f a

instance Traversable Identity where
 traverse f (Identity a) = fmap Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
 arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
 (=-=) = eq


type TI = Identity

main = do
      let trigger :: TI (Sum Int, Product Int, Int, Sum Int)
          trigger = undefined
      quickBatch (traversable trigger)
