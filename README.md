## Alpha Vantage haskell http client


My first attempt to create Alpha Vantage client https://www.alphavantage.co

Supported operations:
Stock Time Series - Daily, Weekly, Monthly  

Example of usage:

```
{-# LANGUAGE OverloadedStrings #-}
module Main where


import Network.HTTP.Client
import Network.HTTP.Client.TLS
import AlphaVantage

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  config <- withStdoutLogging =<< (newConfig $ Apikey "Demo")
  let tsRequest = getStockTimeSeries 
                  StockTimeSeriesMonthly 
                  (Symbol "MSFT") 
                  -&- Outputsize'Compact
  response <- dispatchRequest mgr config tsRequest
  putStrLn $ show response
  return ()
```  
