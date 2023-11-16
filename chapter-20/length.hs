
lgth :: (Foldable t) => t a -> Int
lgth = foldr auxLgth 0
       where auxLgth x 0 = 1
             auxLgth _ y = y + 1
