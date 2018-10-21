{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module AlphaVantage.Core where

import AlphaVantage.Logging

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import qualified Data.Foldable as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time as TI
import qualified Lens.Micro as L
import qualified Network.HTTP.Types as NH
import qualified Prelude as P
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH
import qualified Text.Printf as T

import Data.Function ((&))
import Prelude (($), (.), (<$>),  Maybe(..), String, fmap, return, show, IO, Functor)

-- ** Apikey
newtype Apikey = Apikey { unApikey :: T.Text } deriving (P.Eq, P.Show)

-- * AlphaVantageConfig

data AlphaVantageConfig = AlphaVantageConfig
  { configHost  :: BCL.ByteString -- ^ host supplied in the Request
  , configApiKey :: Apikey
  , configLogExecWithContext :: LogExecWithContext -- ^ Run a block using a Logger instance
  , configLogContext :: LogContext -- ^ Configures the logger
  }

-- | display the config
instance P.Show AlphaVantageConfig where
  show c =
    T.printf
      "{ configHost = %v, ..}"
      (show (configHost c))
    

-- | constructs a default AlphaVantageConfig
--
-- configHost:
--
-- @https://www.alphavantage.co@
--
--
newConfig :: Apikey -> IO AlphaVantageConfig
newConfig apikey = do
    logCxt <- initLogContext
    return $ AlphaVantageConfig
        { configHost = "https://www.alphavantage.co"
        , configApiKey = apikey
        , configLogExecWithContext = runDefaultLogExecWithContext
        , configLogContext = logCxt
        }

-- | updates the config to use stdout logging
withStdoutLogging :: AlphaVantageConfig -> IO AlphaVantageConfig
withStdoutLogging p = do
    logCxt <- stdoutLoggingContext (configLogContext p)
    return $ p { configLogExecWithContext = stdoutLoggingExec, configLogContext = logCxt }

-- | updates the config to use stderr logging
withStderrLogging :: AlphaVantageConfig -> IO AlphaVantageConfig
withStderrLogging p = do
    logCxt <- stderrLoggingContext (configLogContext p)
    return $ p { configLogExecWithContext = stderrLoggingExec, configLogContext = logCxt }

-- | updates the config to disable logging
withNoLogging :: AlphaVantageConfig -> AlphaVantageConfig
withNoLogging p = p { configLogExecWithContext =  runNullLogExec}

-- * AlphaVantageRequest

-- | Represents a request.
--
--   Type Variables:
--
--   * req - request operation
--   * res - response model
data AlphaVantageRequest req res = AlphaVantageRequest
  { rMethod  :: NH.Method   -- ^ Method of AlphaVantageRequest
  , rUrlPath :: [BCL.ByteString] -- ^ Endpoint of AlphaVantageRequest
  , rParams   :: Params -- ^ params of AlphaVantageRequest
  }
  deriving (P.Show)


-- | 'rParams' Lens
rParamsL :: Lens_' (AlphaVantageRequest req res) Params
rParamsL f AlphaVantageRequest{..} = (\p -> AlphaVantageRequest { rParams = p, ..} ) <$> f rParams
{-# INLINE rParamsL #-}

-- * HasOptionalParam

-- | Designates the optional parameters of a request
class HasOptionalParam req param where
  {-# MINIMAL applyOptionalParam | (-&-) #-}

  -- | Apply an optional parameter to a request
  applyOptionalParam :: AlphaVantageRequest req res -> param -> AlphaVantageRequest req  res 
  applyOptionalParam = (-&-)
  {-# INLINE applyOptionalParam #-}

  -- | infix operator \/ alias for 'addOptionalParam'
  (-&-) :: AlphaVantageRequest req res -> param -> AlphaVantageRequest req res 
  (-&-) = applyOptionalParam
  {-# INLINE (-&-) #-}

infixl 2 -&-

-- | Request Params
data Params = Params
  { paramsQuery :: NH.Query
  , paramsHeaders :: NH.RequestHeaders
  }
  deriving (P.Show)

-- | 'paramsQuery' Lens
paramsQueryL :: Lens_' Params NH.Query
paramsQueryL f Params{..} = (\p -> Params { paramsQuery = p, ..} ) <$> f paramsQuery
{-# INLINE paramsQueryL #-}

-- | 'paramsHeaders' Lens
paramsHeadersL :: Lens_' Params NH.RequestHeaders
paramsHeadersL f Params{..} = (\p -> Params { paramsHeaders = p, ..} ) <$> f paramsHeaders
{-# INLINE paramsHeadersL #-}

-- ** AlphaVantageRequest Utils
_mkRequest :: NH.Method -- ^ Method
          -> [BCL.ByteString] -- ^ Endpoint
          -> AlphaVantageRequest req res -- ^ req: Request Type, res: Response Type
_mkRequest m u = AlphaVantageRequest m u _mkParams 

_mkParams :: Params
_mkParams = Params [] []

setHeader :: AlphaVantageRequest req res -> [NH.Header] -> AlphaVantageRequest req res
setHeader req header =
  req `removeHeader` P.fmap P.fst header &
  L.over (rParamsL . paramsHeadersL) (header P.++)

removeHeader :: AlphaVantageRequest req res -> [NH.HeaderName] -> AlphaVantageRequest req res
removeHeader req header =
  req &
  L.over
    (rParamsL . paramsHeadersL)
    (P.filter (\h -> cifst h `P.notElem` P.fmap CI.mk header))
  where
    cifst = CI.mk . P.fst


setQuery :: AlphaVantageRequest req res -> [NH.QueryItem] -> AlphaVantageRequest req res
setQuery req query =
  req &
  L.over
    (rParamsL . paramsQueryL)
    ((query P.++) . P.filter (\q -> cifst q `P.notElem` P.fmap cifst query))
  where
    cifst = CI.mk . P.fst


-- ** Params Utils

toPath
  :: WH.ToHttpApiData a
  => a -> BCL.ByteString
toPath = BB.toLazyByteString . WH.toEncodedUrlPiece

toHeader :: WH.ToHttpApiData a => (NH.HeaderName, a) -> [NH.Header]
toHeader x = [fmap WH.toHeader x]

toForm :: WH.ToHttpApiData v => (BC.ByteString, v) -> WH.Form
toForm (k,v) = WH.toForm [(BC.unpack k,v)]

toQuery :: WH.ToHttpApiData a => (BC.ByteString, Maybe a) -> [NH.QueryItem]
toQuery x = [(fmap . fmap) toQueryParam x]
  where toQueryParam = T.encodeUtf8 . WH.toQueryParam

-- ** Date Utils
parseDate :: P.String -> P.Either P.String TI.UTCTime
parseDate = TI.parseTimeM P.False TI.defaultTimeLocale (TI.iso8601DateFormat P.Nothing)

type Lens_' s a = Lens_ s s a a
type Lens_ s t a b = forall (f :: * -> *). Functor f => (a -> f b) -> s -> f t


class ResponseParser t where 
  parseResponse :: BL.ByteString -> P.Either String t   
