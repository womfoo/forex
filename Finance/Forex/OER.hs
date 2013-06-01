{-# LANGUAGE OverloadedStrings #-}
module Finance.Forex.OER (OERQuery(..)) where

import           Finance.Forex.Types
import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Lazy          as HM
import qualified Data.Text                  as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.HTTP.Conduit

data OERQuery = OERQuery T.Text

instance Query OERQuery where
  url (OERQuery appid)
    = "http://openexchangerates.org/api/latest.json?app_id=" ++ T.unpack appid
  respHandler _ = decode . responseBody

instance FromJSON Quote where
  parseJSON (Object o) =
    do o' <- o .: "rates"
       base <- o .: "base"
       Quote <$> integerToUTC <$> o .: "timestamp"
             <*> parseRates base o'
  parseJSON _  = empty

parseRates :: T.Text -> Value -> Parser [Rate]
parseRates base (Object o) =
  mapM (\(k,v) -> (Rate <$> pure (T.append base k)
                        <*> parseJSON v
                        <*> pure Nothing
                        <*> pure Nothing)
       ) (HM.toList o)
parseRates _ _ = empty

integerToUTC :: Integer -> UTCTime
integerToUTC = posixSecondsToUTCTime . fromIntegral
