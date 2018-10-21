{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module AlphaVantage.Client (
  dispatchRequest
  ) where

import AlphaVantage.Core
import AlphaVantage.Logging

import qualified Control.Exception.Safe as E
import qualified Control.Monad.IO.Class as P
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Types as NH
import qualified Web.HttpApiData as WH

import Data.Monoid ((<>))

-- * Dispatch

-- ** Lbs

-- | send a request returning the raw http response
dispatchLbs
  :: NH.Manager -- ^ http-client Connection manager
  -> AlphaVantageConfig -- ^ config
  -> AlphaVantageRequest req res -- ^ request
  -> IO (NH.Response BCL.ByteString) -- ^ response
dispatchLbs manager config request  = do
  initReq <- _toInitRequest config request
  dispatchInitUnsafe manager config initReq

-- ** Mime

-- | pair of decoded http body and http response
data MimeResult res =
  MimeResult { mimeResult :: Either MimeError res -- ^ decoded http body
             , mimeResultResponse :: NH.Response BCL.ByteString -- ^ http response
             }
  deriving (Show, Functor, Foldable, Traversable)

-- | pair of unrender/parser error and http response
data MimeError =
  MimeError {
    mimeError :: String -- ^ unrender/parser error
  , mimeErrorResponse :: NH.Response BCL.ByteString -- ^ http response
  } deriving (Eq, Show)

-- | send a request returning the 'MimeResult'
dispatchRequest
  :: ResponseParser res => NH.Manager -- ^ http-client Connection manager
  -> AlphaVantageConfig -- ^ config
  -> AlphaVantageRequest req res -- ^ request
  -> IO (Either MimeError res) -- ^ response
dispatchRequest manager config request = do
  httpResponse <- dispatchLbs manager config request
  let statusCode = NH.statusCode . NH.responseStatus $ httpResponse
  runConfigLogWithExceptions "Client" config $
    if statusCode >= 400 && statusCode < 600
       then do
         let s = "error statusCode: " ++ show statusCode
         _log "Client" levelError (T.pack s)
         pure (Left (MimeError s httpResponse))
       else case parseResponse (NH.responseBody httpResponse) of
         Left s -> do
           _log "Client" levelError (T.pack s)
           pure (Left (MimeError s httpResponse))
         Right r -> pure (Right r)


-- | dispatch an InitRequest
dispatchInitUnsafe
  :: NH.Manager -- ^ http-client Connection manager
  -> AlphaVantageConfig -- ^ config
  -> InitRequest req res -- ^ init request
  -> IO (NH.Response BCL.ByteString) -- ^ response
dispatchInitUnsafe manager config (InitRequest req) =
  runConfigLogWithExceptions src config $
    do _log src levelInfo requestLogMsg
       _log src levelDebug requestDbgLogMsg
       res <- P.liftIO $ NH.httpLbs req manager
       _log src levelInfo (responseLogMsg res)
       _log src levelDebug ((T.pack . show) res)
       return res
  where
    src = "Client"
    endpoint =
      T.pack $
      BC.unpack $
      NH.method req <> " " <> NH.host req <> NH.path req <> NH.queryString req
    requestLogMsg = "REQ:" <> endpoint
    requestDbgLogMsg =
      "Headers=" <> (T.pack . show) (NH.requestHeaders req) <> " Body=" <>
      (case NH.requestBody req of
         NH.RequestBodyLBS xs -> T.decodeUtf8 (BL.toStrict xs)
         _ -> "<RequestBody>")
    responseStatusCode = (T.pack . show) . NH.statusCode . NH.responseStatus
    responseLogMsg res =
      "RES:statusCode=" <> responseStatusCode res <> " (" <> endpoint <> ")"

-- * InitRequest

-- | wraps an http-client 'Request' with request/response type parameters
newtype InitRequest req res = InitRequest
  { unInitRequest :: NH.Request
  } deriving (Show)

-- |  Build an http-client 'Request' record from the supplied config and request
_toInitRequest
  :: AlphaVantageConfig -- ^ config
  -> AlphaVantageRequest req res -- ^ request
  -> IO (InitRequest req res) -- ^ initialized request
_toInitRequest config req0  =
  runConfigLogWithExceptions "Client" config $ do
    parsedReq <- P.liftIO $ NH.parseRequest $ BCL.unpack $ BCL.append (configHost config) (BCL.concat (rUrlPath req0))
    let req1 = req0 
               `setQuery` toQuery ("datatype", Just ("csv" :: T.Text))
               `setQuery` toQuery ("apikey", Just $ unApikey $ configApiKey $ config)
        reqHeaders = ("User-Agent", WH.toHeader (T.pack "alpha-vantage-hs")) : paramsHeaders (rParams req1)
        reqQuery = NH.renderQuery True (paramsQuery (rParams req1))
        pReq = parsedReq { NH.method = rMethod req1
                        , NH.requestHeaders = reqHeaders
                        , NH.queryString = reqQuery
                        }
    outReq <- pure (pReq { NH.requestBody = mempty })

    pure (InitRequest outReq)

-- ** Logging

-- | Run a block using the configured logger instance
runConfigLog
  :: P.MonadIO m
  => AlphaVantageConfig -> LogExec m
runConfigLog config = configLogExecWithContext config (configLogContext config)

-- | Run a block using the configured logger instance (logs exceptions)
runConfigLogWithExceptions
  :: (E.MonadCatch m, P.MonadIO m)
  => T.Text -> AlphaVantageConfig -> LogExec m
runConfigLogWithExceptions src config = runConfigLog config . logExceptions src
