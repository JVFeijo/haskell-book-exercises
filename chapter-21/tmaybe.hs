import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
 fmap f Nada = Nada
 fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
 foldMap f Nada = mempty
 foldMap f (Yep a) = f a

instance Traversable Optional where
 traverse f Nada = pure Nada
 traverse f (Yep a) = Yep <$> (f a)

instance Arbitrary a => Arbitrary (Optional a) where
 arbitrary = frequency [(1, (pure $ Nada)), (1, (Yep <$> arbitrary))]

instance Eq a => EqProp (Optional a) where
 (=-=) Nada Nada = property True
 (=-=) Nada _ = property False
 (=-=) _ Nada = property False
 (=-=) (Yep a) (Yep b) = property (eq a b)

type TI = Optional

main = do
         let trigger :: TI (Sum Int, Product Int, Int, Sum Int)
             trigger = undefined
         quickBatch (traversable trigger)
