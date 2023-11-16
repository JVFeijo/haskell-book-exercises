module EitherT where

import Control.Applicative

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
 fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance (Applicative m) => Applicative (EitherT e m) where
 pure = EitherT . pure . pure

 (EitherT mef ) <*> (EitherT mea) = EitherT $ liftA2 (<*>) mef mea

instance (Monad m) => Monad (EitherT e m) where
 return = pure

 (EitherT mea) >>= f =
  EitherT $ do
              ea <- mea
              case ea of
                   Left e -> return (Left e)
                   Right a -> (runEitherT . f) a


swapEither' :: Either e a -> Either a e
swapEither' (Left e) = Right e
swapEither' (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ swapEither' <$> ma

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mb) = mb >>= either' f g
