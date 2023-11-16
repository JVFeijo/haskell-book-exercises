import Data.Maybe
mini :: (Foldable f, Ord a) => f a -> Maybe a
mini xs = if (length xs == 0) then Nothing
            else foldr maybeMin Nothing xs
           where
                maybeMin x Nothing = Just x
                maybeMin x (Just y) = Just (min x y) 
