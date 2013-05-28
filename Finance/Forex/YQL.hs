{-# LANGUAGE OverloadedStrings #-}
module Finance.Forex.YQL (yahooRate) where

import           Finance.Forex.Types
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import           Data.List
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Network.HTTP.Conduit
import           Network.HTTP.Types    (renderSimpleQuery)
import           Safe

yahooRate :: T.Text -> [T.Text] -> IO (Maybe Quote)
yahooRate base counters =  do
  request <- yahooRequest base counters
  response <- withManager . httpLbs $ request
  return $ decode . responseBody $ response

yahooRequest base counters = do
  request <- parseUrl $ B.unpack url
  return $ request { checkStatus = \_ _ _ -> Nothing }
  where base' = T.encodeUtf8 base
        counters' = map T.encodeUtf8 counters
        url   = B.concat ["http://query.yahooapis.com/v1/public/yql"
                         ,renderSimpleQuery True parms]
        parms = [("q",yql)
                ,("env","store://datatables.org/alltableswithkeys")
                ,("format","json")]
        yql   = B.concat ["select * from yahoo.finance.xchange where pair in ("
                         , B.concat ( intersperse "," pairs)
                         ,")"]
        pairs = map (\c -> B.concat ["\"",base',c,"\""]) counters'

instance FromJSON Quote where
  parseJSON (Object o) = do
    o' <- (.: "rate") =<< (.: "results") =<< o .: "query"
    Quote <$> ((.: "created") =<< o .: "query") <*> mapM parseJSON o'
  parseJSON _ = mzero

instance FromJSON Rate where
  parseJSON (Object o) =
    Rate <$> (o .: "id")
         <*> (textToDouble <$> o .:? "Rate")
         <*> (textToDouble <$> o .:? "Ask" )
         <*> (textToDouble <$> o .:? "Bid" )
  parseJSON _ = mzero

textToDouble :: Maybe T.Text -> Maybe Double
textToDouble x = x >>= readMay . T.unpack
