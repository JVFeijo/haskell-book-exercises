module StateT where

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
 fmap f (StateT sma) = StateT $ (fmap . fmap) ((,) <$> (f . fst) <*> snd) sma


instance (Monad m) => Applicative (StateT s m) where
 pure a = StateT $ pure <$> (,) a

 (StateT smf) <*> (StateT sma) = StateT $ \s1 -> (sma s1) >>= \(a, s2) -> (smf s2) >>= \(f, s3) -> return (f a, s3)


instance (Monad m) => Monad (StateT s m) where
 return = pure

 (StateT sma) >>= f = StateT $ \s1 -> (sma s1) >>= \(a, s2) -> runStateT (f a) s2
