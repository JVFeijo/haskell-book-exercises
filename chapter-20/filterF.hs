
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF check ts = foldMap aux ts
                      where aux x = if (check x) then (pure x)
                                    else mempty
