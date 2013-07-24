{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import Blaze.ByteString.Builder.ByteString (fromByteString)
import Control.Concurrent
import Control.Exception.Base (catch)
import Control.Monad.IO.Class
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString as BS (concat)
import Data.ByteString.Internal (ByteString)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Conduit.Lazy (lazyConsume)
import Data.Monoid
import Data.Time
import Network.HTTP.Conduit as H
import Network.Wai as W
import Network.Wai.Handler.Warp
import Prelude hiding (concat)
import System.Exit
import System.Locale
import System.IO
import Control.Applicative ( (<$>) )


pharmash = "http://hal" :: ByteString


sigHandler mv = do putMVar mv True

getParam [] _   = Nothing
getParam (_:[]) _ = Nothing
getParam (a:args) flag
    | a == flag = Just $ head args
    | otherwise = getParam args flag


orElse m b = case m of
    Just a  -> a
    Nothing -> b

orDie m s = case m of
    Just a -> return a
    Nothing -> do
        putStrLn s
        exitFailure

proxApp sourcePort req = do
    liftIO $ logReq req
    pRequestBase <- parseUrl $ unpack $ pharmash `mappend`  rawPathInfo req
                                                `mappend`  rawQueryString req
    rBody <- liftIO $ mconcat <$> C.runResourceT (W.requestBody req C.$$ CL.consume)
    {-liftIO $ putStrLn $ show rBody-}
    let pRequest = pRequestBase { H.requestBody = RequestBodyBS (rBody)
                                , H.method      = requestMethod req
                                , H.requestHeaders = W.requestHeaders req
                                , H.port        = sourcePort
                                }
    (return =<< liftIO) $ catch
        (H.withManager $ \manager -> do
            res <- http pRequest manager
            let body = (H.responseBody res)
            b <- body C.$$+- CL.consume
            return $ ResponseBuilder (H.responseStatus res) (H.responseHeaders res) ( fromByteString $ BS.concat b) )
        (\(StatusCodeException status headers jar) -> do
           return $ ResponseBuilder status headers $ fromByteString "not ok")


logReq req = do
    time <- getCurrentTime
    localeTime <- utcToLocalZonedTime time
    rBody <- liftIO . C.runResourceT . lazyConsume . W.requestBody $ req
    let timeString = formatTime defaultTimeLocale "[ %D %X%Q ]\t" localeTime
    putStrLn $ timeString
               ++ (show $ requestMethod req)
               {-++ " -- " ++ (show $ httpVersion req)-}
               {-++ " -- " ++ (show $ serverName req)-}
               {-++ " -- " ++ (show $ rawPathInfo req)-}
               ++ " -- " ++ (show $ remoteHost req)
               {-++ " -- " ++ (show $ W.queryString req)-}
               ++ " -- " ++ (show $ W.requestHeaders req)
               ++ " -- " ++ (show rBody)
    hFlush stdout

main = do
    {-args <- getArgs-}
    {-logFile <- orDie (getParam args "-l") "Missing log output file"-}
    run 15080 (proxApp 15080)




