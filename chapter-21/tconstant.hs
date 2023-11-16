import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
 fmap f (Constant a) = Constant a

instance Foldable (Constant a) where
 foldMap f (Constant a) = mempty

instance Traversable (Constant a) where
 traverse f (Constant a) = pure (Constant a)

instance Arbitrary a => Arbitrary (Constant a b) where
 arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
 (=-=) = eq

type TI = Constant Int

main = do
         let trigger :: TI (Sum Int, Product Int, Int, Sum Int)
             trigger = undefined
         quickBatch (traversable trigger)
