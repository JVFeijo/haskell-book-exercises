

fldmp :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
fldmp f ts = foldr (\x acc -> (f x) <> acc) mempty ts
