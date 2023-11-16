
data Constant a b = Constant b

instance Foldable (Constant a) where
 foldr f acc (Constant a) = f a acc
