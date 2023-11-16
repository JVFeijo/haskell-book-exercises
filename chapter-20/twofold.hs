
data Two a b = Two a b

instance Foldable (Two a) where
 foldr f acc (Two a b) = f b acc
