{-# LANGUAGE OverloadedStrings #-}
module Finance.Forex.OER (oerRate) where

import           Finance.Forex.Types
import           Control.Applicative
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Lazy          as HM
import qualified Data.Text                  as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.HTTP.Conduit

oerRate :: T.Text -> IO (Maybe Quote)
oerRate appid =  do
  request <- parseUrl $ "http://openexchangerates.org/api/latest.json?app_id=" ++ T.unpack appid
  response <- withManager . httpLbs $ request { checkStatus = \_ _ _ -> Nothing }
  return $ decode . responseBody $ response

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
