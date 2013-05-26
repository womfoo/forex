module Finance.Forex.Types where

import Data.Text
import Data.Time

data Quote = Quote {timestamp :: UTCTime
                   ,rates :: [Rate]
                   } deriving Show

data Rate = Rate {pair :: Text
                 ,rate :: Maybe Double
                 ,ask  :: Maybe Double
                 ,bid  :: Maybe Double
                  } deriving Show
