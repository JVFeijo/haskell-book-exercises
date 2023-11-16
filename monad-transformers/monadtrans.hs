{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Trans.Class
import Control.Monad
import EitherT
import StateT

instance MonadTrans (EitherT e) where
-- lift :: Monad m => m a -> t m a
 lift = EitherT . liftM Right

instance MonadTrans (StateT s) where
 lift ma = StateT $ \s -> liftM (flip (,) s) ma
