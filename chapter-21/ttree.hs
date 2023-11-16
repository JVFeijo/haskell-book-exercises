import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
 deriving (Eq, Show)

instance Functor Tree where
 fmap f Empty = Empty
 fmap f (Leaf a) = Leaf (f a)
 fmap f (Node t1 a t2) = Node (fmap f t1) (f a) (fmap f t2)

instance Foldable Tree where
 foldMap f Empty = mempty
 foldMap f (Leaf a) = f a
 foldMap f (Node t1 a t2) = (foldMap f t1) <> (f a) <> (foldMap f t2)

instance Traversable Tree where
 traverse f Empty = pure Empty
 traverse f (Leaf a) = Leaf <$> (f a)
 traverse f (Node t1 a t2) = Node <$> (traverse f t1) <*> (f a) <*> (traverse f t2)

instance Arbitrary a => Arbitrary (Tree a) where
 arbitrary = sized $ \n ->
   if n < 1
      then pure Empty
      else if n == 1
              then (Leaf <$> arbitrary)
              else
                oneof [ pure Empty,
                        Leaf <$> arbitrary,
                        Node <$> resize (n `div` 2) arbitrary <*> arbitrary <*> resize (n `div` 2) arbitrary
                      ]

instance Eq a => EqProp (Tree a) where
 (=-=) = eq


type TI = Tree

main = do
      let trigger :: TI (Sum Int, Product Int, Int, Sum Int)
          trigger = undefined
      quickBatch (traversable trigger)
