{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AlphaVantage.Model where

import AlphaVantage.Core

import qualified Data.ByteString.Char8 as BC
import qualified Data.Data as P (Typeable)
import qualified Data.Time as TI
import qualified Web.HttpApiData as WH

import Data.Text (Text)

import qualified Prelude as P
import Prelude (($), (.), (<$>), (<*>))
import Data.Csv
import qualified Control.Monad.Fail as Fail
import qualified Data.Vector as V

-- * Parameter newtypes

-- ** Symbol
newtype Symbol = Symbol { unSymbol :: Text } deriving (P.Eq, P.Show)

-- * Models

data TimeSeries a = TimeSeries
  {
    timeSeriesData :: !(V.Vector (TI.UTCTime, a))
  } deriving (P.Show, P.Eq, P.Typeable)

data MarketData = MarketData
  {
    marketDataOpen :: !P.Double -- ^ /Required/ "Time Series"
   , marketDataHigh :: !P.Double -- ^ /Required/ "Time Series"
   , marketDataLow :: !P.Double -- ^ /Required/ "Time Series"
   , marketDataClose :: !P.Double -- ^ /Required/ "Time Series"
   , marketDataVolume :: !P.Double -- ^ /Required/ "Time Series"
  } deriving (P.Show, P.Eq, P.Typeable)



instance FromField TI.UTCTime where
  parseField s = case parseDate $ BC.unpack s of
    P.Right r  -> P.pure r
    P.Left err -> Fail.fail err

instance FromNamedRecord (TI.UTCTime, MarketData) where
  parseNamedRecord r =
    (,)
      <$> r .:  "timestamp"
      <*> (   MarketData
          <$> r .:  "open"
          <*> r .:  "high"
          <*> r .:  "low"
          <*> r .:  "close"
          <*> r .:  "volume"
          )


instance ResponseParser (TimeSeries MarketData) where 
  parseResponse res = case decodeByName res of
    P.Left err ->  P.Left err
    P.Right (_, v) -> P.Right $  TimeSeries v        


data Outputsize
  = Outputsize'Compact -- ^ @"compact"@
  | Outputsize'Full -- ^ @"full"@
  deriving (P.Show, P.Eq, P.Typeable, P.Ord, P.Bounded, P.Enum)

instance WH.ToHttpApiData Outputsize where toQueryParam = WH.toQueryParam . fromOutputsize

-- | unwrap 'E'Outputsize' enum
fromOutputsize :: Outputsize -> Text
fromOutputsize = \case
  Outputsize'Compact -> "compact"
  Outputsize'Full -> "full"

-- | parse 'E'Outputsize' enum
toOutputsize :: Text -> P.Either P.String Outputsize
toOutputsize = \case
  "compact" -> P.Right Outputsize'Compact
  "full" -> P.Right Outputsize'Full
  s -> P.Left $ "toOutputsize: enum parse failure: " P.++ P.show s

data StockTimeSeries
  = StockTimeSeriesDaily
  | StockTimeSeriesWeekly
  | StockTimeSeriesMonthly
  deriving (P.Show, P.Eq, P.Typeable)

data TechnicalIndicator
  = TechnicalIndicatorSAR { technicalIndicatorSarInterval :: Interval }
  deriving (P.Show, P.Eq, P.Typeable)


data Interval
  = Interval'1min
  | Interval'5min
  | Interval'15min
  | Interval'30min
  | Interval'60min
  | Interval'daily
  | Interval'weekly
  | Interval'monthly
  deriving (P.Show, P.Eq, P.Typeable)
