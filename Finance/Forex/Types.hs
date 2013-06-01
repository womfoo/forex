module Finance.Forex.Types where

import Data.ByteString.Lazy 
import Data.Text
import Data.Time
import Network.HTTP.Conduit

class Query q where
  url         :: q -> String
  respHandler :: q -> (Response ByteString) -> Maybe Quote

data Quote = Quote {timestamp :: UTCTime
                   ,rates :: [Rate]
                   } deriving Show

data Rate = Rate {pair :: Text
                 ,rate :: Maybe Double
                 ,ask  :: Maybe Double
                 ,bid  :: Maybe Double
                  } deriving Show
