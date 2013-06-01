module Finance.Forex
    (
      getQuote
    , GoogQuery(..)
    , OERQuery(..)
    , YQLQuery(..)
    ) where
import Finance.Forex.GoogleCalc
import Finance.Forex.OER
import Finance.Forex.YQL
import Finance.Forex.Types
import Network.HTTP.Conduit

getQuote :: (Query q) => q -> IO (Maybe Quote)
getQuote q =  do
  url' <- parseUrl $ url q
  response <- withManager . httpLbs $ url'
  return $ (respHandler q) response
