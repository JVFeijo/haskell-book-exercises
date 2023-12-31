import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = ReaderT $ return . (flip (-) 1)

rShow :: Show a => ReaderT a Identity String
rShow = reader show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
                                 print ("Hi: " ++ show r)
                                 return (r+1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
                                  print ("Hi: " ++ show s)
                                  return (show s, s+1)
