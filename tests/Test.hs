{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import Instances ()

import AlphaVantage.Model


main :: IO ()
main =
  hspec $ modifyMaxSize (const 5) $ do
    describe "JSON instances" $ do
      pure ()
    
      
