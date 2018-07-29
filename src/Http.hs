module Http (HasManager, http) where

import Control.Monad.Reader.Class
import Control.Monad.Trans.Resource (MonadResource)
    
import Network.HTTP.Client (Request, Response, Manager)
import qualified Network.HTTP.Conduit as NHC
import Data.Conduit (ResumableSource)
import Data.ByteString (ByteString)
    
class HasManager r where
    manager :: r -> Manager

instance HasManager Manager where
    {-# INLINE manager #-}
    manager = id

http :: (MonadReader r m, HasManager r,
         MonadResource m) =>
        Request -> m (Response (ResumableSource m ByteString))
http req = asks manager >>= NHC.http req
