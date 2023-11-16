
data Three a b = Three a b b

instance Foldable (Three a) where
 foldr f acc (Three a b1 b2) = f b2 (f b1 acc)
