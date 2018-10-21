{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module AlphaVantage.API.MarketData where

import AlphaVantage.Core
import AlphaVantage.Model as M

import Data.Text (Text)


getStockTimeSeries
  :: StockTimeSeries -- ^ "function"
  -> Symbol -- ^ "symbol" -  The stocker ticker symbol
  -> AlphaVantageRequest GetStockTimeSeries (TimeSeries MarketData)
getStockTimeSeries f (Symbol symbol) =
  case f of
        StockTimeSeriesDaily -> request `setQuery` toQuery ("function", Just  ("TIME_SERIES_DAILY" :: Text))
        StockTimeSeriesWeekly -> request `setQuery` toQuery ("function", Just  ("TIME_SERIES_WEEKLY" :: Text))
        StockTimeSeriesMonthly -> request `setQuery` toQuery ("function", Just  ("TIME_SERIES_MONTHLY" :: Text))
  where request = _mkRequest "GET" ["/query"]
          `setQuery` toQuery ("symbol", Just symbol)

data GetStockTimeSeries
instance HasOptionalParam GetStockTimeSeries Outputsize where
  applyOptionalParam req xs =
    req `setQuery` toQuery ("outputsize", Just xs)
