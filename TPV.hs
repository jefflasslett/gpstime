{-# LANGUAGE OverloadedStrings #-}
module TPV where

import Control.Applicative
import Data.Aeson

data TPV =
  TPV { klass  :: String
      , tag    :: Maybe String
      , device :: Maybe String
      , mode   :: Int
      , time   :: Maybe String
      , ept    :: Maybe Double
      , lat    :: Maybe Double
      , lon    :: Maybe Double
      , alt    :: Maybe Double
      , epx    :: Maybe Double
      , epy    :: Maybe Double
      , epv    :: Maybe Double
      , track  :: Maybe Double
      , speed  :: Maybe Double
      , climb  :: Maybe Double
      , epd    :: Maybe Double
      , eps    :: Maybe Double
      , epc    :: Maybe Double
      } deriving ( Show )

instance FromJSON TPV where
  parseJSON ( Object v ) =
    TPV <$>
      v .: "class" <*>
      v .:? "tag" <*>
      v .:? "device" <*>
      v .: "mode" <*>
      v .:? "time" <*>
      v .:? "ept" <*>
      v .:? "lat" <*>
      v .:? "lon" <*>
      v .:? "alt" <*>
      v .:? "epx" <*>
      v .:? "epy" <*>
      v .:? "epv" <*>
      v .:? "track" <*>
      v .:? "speed" <*>
      v .:? "climb" <*>
      v .:? "epd" <*>
      v .:? "eps" <*>
      v .:? "epc"

  parseJSON _ = empty


