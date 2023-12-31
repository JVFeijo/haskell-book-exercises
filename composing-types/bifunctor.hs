
class Bifunctor p where
 {-# MINIMAL bimap | first, second #-}
 bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
 bimap f g = first f . second g
 
 first :: (a -> b) -> p a c -> p b c
 first f = bimap f id
 
 second :: (b -> c) -> p a b -> p a c
 second = bimap id 


data Deux a b = Deux a b

instance Bifunctor Deux where
 bimap f g (Deux x y) = Deux (f x) (g y)

data Const a b = Const a

instance Bifunctor Const where
 bimap f _ (Const a) = Const (f a)

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
 bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
 bimap f _ (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
 bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
 bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Eitherr a b = Leftt a | Rightt b

instance Bifunctor Eitherr where
 bimap f _ (Leftt a) = Leftt (f a)
 bimap _ g (Rightt b) = Rightt (g b)
