module ReaderT where

import Control.Applicative

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
 fmap f (ReaderT rma) = ReaderT $ (fmap f) . rma

instance (Applicative m) => Applicative (ReaderT r m) where
 pure a = ReaderT $ \r -> pure a

 (ReaderT rmf) <*> (ReaderT rma) = ReaderT $ (<*>) <$> rmf <*> rma

instance (Monad m) => Monad (ReaderT r m) where
 return = pure

 (ReaderT rma) >>= f = ReaderT $ (>>=) <$> rma <*> (flip (runReaderT . f))
