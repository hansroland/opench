{-# LANGUAGE OverloadedStrings #-}

-- | URLs for the Swiss Meteo Net API
module Data.Meteo.Swiss.Urls
  (
  -- * Functions that return URLs
    urlDataAll
  , urlDataStat
  , urlDocuStat   
  )
where

import qualified Data.Text as T
import           Data.Monoid ((<>))

-- | Return the URL to retrieve all meteo data from all stations for the last 10 minutes.
urlDataAll :: T.Text
urlDataAll = "https://opendata.netcetera.com/smn/smn/"

-- | Return the URL to retrieve the meteo data for a given station for the last 10 minutes.
urlDataStat :: T.Text    -- ^ 3-char all upper-case station code
  -> T.Text
urlDataStat stat = "https://opendata.netcetera.com/smn/smn/" <> stat

-- | Return the URL to retrieve the documentation sheet for a given station
urlDocuStat :: T.Text    -- ^ 3-char all upper-case station code 
  -> T.Text
urlDocuStat stat = "http://www.meteoschweiz.admin.ch/product/input/smn-stations/docs/" <> stat <> ".pdf"