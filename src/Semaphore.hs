module Semaphore (
                  HasSemaphore,
                  withSemaphore
                 )
where

import Control.Concurrent.QSem
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Control.Monad.Catch
    
class HasSemaphore r where
    semaphore :: r -> QSem

instance HasSemaphore QSem where
    {-# INLINE semaphore #-}
    semaphore = id


{-# INLINABLE withSemaphore #-}
withSemaphore :: (MonadReader r m, HasSemaphore r,
                  MonadMask m, MonadIO m) => m a -> m a
withSemaphore action = do
  sem <- asks semaphore
  bracket_ (liftIO $ waitQSem sem) (liftIO $ signalQSem sem) action
