import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import MaybeT
import ReaderT
import StateT

instance MonadIO m => MonadIO (MaybeT m) where
 liftIO = MaybeT . (fmap Just) . liftIO


instance MonadIO m => MonadIO (ReaderT r m) where
 liftIO a = ReaderT $ \r -> liftIO a

instance MonadIO m => MonadIO (StateT s m) where
 liftIO a = StateT $ \s ->  (flip (,) s) <$> (liftIO a)
