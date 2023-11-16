

tlst :: (Foldable t) => t a -> [a]
tlst = foldr auxLst []
       where auxLst x [] = [x]
             auxLst x xs = x:xs
