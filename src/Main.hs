{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Main where

import System.Timeout
import qualified Data.Map.Strict as M
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Data.Maybe
    
import System.Directory
import Text.Printf
import qualified Data.Text as T
import Network.URI (URI, parseRelativeReference, uriPath)

import Control.Monad.Trans.Resource (runResourceT, MonadResource)
import qualified Data.Conduit as C
import Data.Conduit.Binary (sinkFile)
    
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
    
import Text.HTML.DOM (sinkDoc) 
import Network.HTTP.Conduit hiding (http)
import Network.HTTP.Types

import Text.XML (Document())
import Text.XML.Cursor (fromDocument,
                        attributeIs,
                        attribute,
                        element, content,
                        ($//), (&/))

import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.QSem

import Semaphore
import Http
    
data IndexData a = IndexData {
      imgNum  :: !Int           
    , imgYear :: !String
    , imgVal  :: !a
    }

data ImageInfo m v = ImageInfo {
      getNamer  :: !(Int -> String)
    , getSource :: !(C.ResumableSource m v)
    }
                   
instance Functor IndexData where
    {-# INLINE fmap #-}
    f `fmap` (IndexData a b c) = IndexData a b (f c)

                                 
simpleReq :: Request
simpleReq = defaultRequest {
              path = "/index.cfm"
            , host = "www.alexcartoon.com"
            }
            

imgReq :: IndexData URI -> Request
imgReq uri = simpleReq {
               path = BC.pack $ uriPath $ imgVal uri
             }


alexRequest :: Int -> Request
alexRequest num = setQueryString [("cartoon_num", Just numStr)]
                  simpleReq
      where
        numStr = BL.toStrict $
                 BB.toLazyByteString $
                 BB.intDec num


indexData :: Int -> Document -> Maybe (IndexData URI)
indexData n doc = IndexData n <$> (T.unpack <$> tmp) <*> uri
    where
      uri    = listToMaybe imgSrc >>=
               parseRelativeReference . T.unpack
      imgSrc = fromDocument doc $//
               element "div" >=>
               attributeIs "class" "strip" &/
               element "img" >=>
               attribute "src"
      tmp    = listToMaybe $ reverse $ T.words year
      year   = T.concat $
               fromDocument doc $//
               element "div" >=> attributeIs "class" "date" &/
               element "h2" &/ content


namer :: BC.ByteString -> Maybe (Int -> String)
namer = fmap printf . flip M.lookup tbl
    where
      tbl = M.fromList [
             ("image/png", "/%04i.png")
            , ("image/jpeg", "/%04i.jpeg")
            , ("image/jpg", "/%04i.jpeg")
            , ("image/gif", "/%04i.gif")
            ]

baseName imageData = imgVal imageData `getNamer` imgNum imageData

imgSource = getSource . imgVal

fetchLoc :: (MonadReader r m, HasManager r,
             MonadResource m) =>
            Int -> m (Maybe (IndexData URI))
fetchLoc n = indexData n <$> do
  liftIO $ putStrLn $ "Fetching " ++ (show n)
  response <- http $ alexRequest n
  responseBody response C.$$+- sinkDoc


fetchImage :: (MonadReader r m, HasManager r,
               MonadResource m) =>
              IndexData URI ->
                  m (Maybe (IndexData (ImageInfo m BC.ByteString)))
fetchImage location = do
  liftIO $ putStrLn $ "Fetching image " ++ show (imgNum location)
  response <- http $ imgReq location
  let ct = lookup hContentType (responseHeaders response) >>= namer
      builder name = location {
                        imgVal = ImageInfo name $ responseBody response
                      }
  return $ builder <$> ct

saveImage :: MonadResource m =>
             IndexData (ImageInfo m BC.ByteString) -> m ()
saveImage imageData = do
  let dir  = "data/" ++ imgYear imageData
      base = baseName imageData
  liftIO $ createDirectoryIfMissing True dir
  imgSource imageData C.$$+- sinkFile (dir ++ base)


{-# INLINE maybeM #-}
maybeM :: Monad m => m (Maybe a) -> m b -> (a -> m b) -> m b
maybeM x def ac = do
  got <- x
  case got of
    Just x -> ac x
    _      -> def

              
{-# INLINE doFetch' #-}
doFetch' :: Monad m =>
            (a -> m (Maybe b)) ->
                (b -> m (Maybe c)) ->
                    (c -> m ()) ->
                        a -> m ()
doFetch' getLoc getPic savePic n = maybeM action (pure ()) savePic
    where
      getLoc' = MaybeT . getLoc
      getPic' = MaybeT . getPic
      action  = runMaybeT $ getLoc' n >>= getPic'

               
doFetch :: (MonadReader r m, HasManager r,
            MonadResource m) => Int -> m ()
doFetch = doFetch' fetchLoc fetchImage saveImage

          
relaunch :: Monad m => m (Maybe b) -> m b
relaunch x = go
    where
      go = maybeM x go pure

           
main :: IO ()
main = do
  semaphore <- newQSem 10
  let baseSettings = tlsManagerSettings {
                       managerConnCount = 10
                     , managerResponseTimeout = Nothing
                     }
  manager   <- newManager baseSettings
  let nums    = [1..8000]
      fetcher = flip runReaderT
  mapConcurrently_ (relaunch .
                    fetcher semaphore .
                    withSemaphore .
                    liftIO . timeout 10000000 .
                    runResourceT .
                    fetcher manager .
                    doFetch) nums
