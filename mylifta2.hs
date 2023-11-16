
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 func fa fb = func <$> fa <*> fb
