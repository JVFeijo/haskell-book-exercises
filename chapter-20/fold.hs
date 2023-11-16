
fld :: (Foldable t, Monoid m) => t m -> m
fld = foldMap id 
