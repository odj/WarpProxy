{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Prelude hiding (catch)

import Network.Wai as W
import Network.Wai.Handler.Warp
import Network.HTTP.Conduit as H
import qualified Data.Conduit as C
import Blaze.ByteString.Builder.ByteString
import Blaze.ByteString.Builder (toByteString)
import Control.Monad.IO.Class
import Control.Exception.Base (catch)
import Data.Monoid
import Data.Time
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Internal (ByteString)
import System.Locale


pharmash = "http://www.pharmash.com" :: ByteString

proxApp req = do
    liftIO $ logReq req
    pRequestBase <- parseUrl $ unpack $ pharmash `mappend`  rawPathInfo req
                                                `mappend`  rawQueryString req
    let requestBodySrc = W.requestBody req
    rBody <- requestBodySrc C.$$ requestSink
    let pRequest = pRequestBase { H.requestBody = RequestBodyBS $ toByteString rBody
                                , H.method      = requestMethod req
                                , H.requestHeaders = W.requestHeaders req
                                , H.port        = 8080
                                }
    (return =<< liftIO) $ catch 
        (H.withManager $ \manager -> do 
            Response status _ headers src <- http pRequest manager
            body <- src C.$$ responseSink
            return $ ResponseBuilder status headers body)
        (\(StatusCodeException status headers) -> do
           return $ ResponseBuilder status headers $ fromByteString "")


responseSink = C.sinkState
    (fromByteString "")
    (\acc a -> return $ C.StateProcessing $ mappend acc $ fromByteString a )
    (\acc -> return acc)


-- NB - same as responseSink for now
requestSink = C.sinkState
    (fromByteString "")
    (\acc a -> return $ C.StateProcessing $ mappend acc $ fromByteString a )
    (\acc -> return acc)


logReq req = do
    time <- getCurrentTime
    localeTime <- utcToLocalZonedTime time
    let timeString = formatTime defaultTimeLocale "[ %D %X%Q ]\t" localeTime
    putStrLn $ timeString
               ++ (show $ requestMethod req)
               {-++ " -- " ++ (show $ httpVersion req)-}
               {-++ " -- " ++ (show $ serverName req)-}
               ++ " -- " ++ (show $ rawPathInfo req)
               ++ " -- " ++ (show $ remoteHost req)
               {-++ " -- " ++ (show $ W.queryString req)-}
               {-++ " -- " ++ (show $ W.requestHeaders req)-}
    
main = do
    run 80 proxApp




