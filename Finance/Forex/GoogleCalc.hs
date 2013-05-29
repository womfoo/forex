{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Finance.Forex.GoogleCalc (googleCalcRate) where

import           Finance.Forex.Types
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Lazy       as HM
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Text.Read
import           Data.Time
import           Network.HTTP.Conduit

googleCalcRate :: T.Text -> T.Text -> IO (Maybe Quote)
googleCalcRate base counter =  do
  request <- parseUrl $ "http://www.google.com/ig/calculator?hl=en&q=1" ++ T.unpack base ++ "%3D%3F" ++ T.unpack counter
  response <- withManager . httpLbs $ request { checkStatus = \_ _ _ -> Nothing }
  now <- getCurrentTime
  let parsedValue = decode . TLE.encodeUtf8 . quoteJSONKeys . TLE.decodeUtf8 . responseBody $ response
      addCurrency r = r {pair = T.append base counter}
  return $ Quote <$> pure now
                 <*> (replicate 1 <$> addCurrency <$> parsedValue)

instance FromJSON Rate where
  parseJSON (Object o)  | Just "" <- HM.lookup "error" o
                        = Rate <$> pure "" -- temporary placeholder for currency
                               <*> (doubleMay <$> removeSpaces <$> o .: "rhs")
                               <*> pure Nothing
                               <*> pure Nothing
  parseJSON _ = mzero

-- Google returns non-compliant JSON, enclose keys with quotes
quoteJSONKeys rawjson = foldl quoteJSONKey rawjson ["lhs","rhs","error","icc"]

quoteJSONKey rawjson key = TL.replace brokenKey quotedKey rawjson
   where brokenKey = TL.concat [key,": "]
         quotedKey = TL.concat ["\"",key,"\": "]

doubleMay :: T.Text -> Maybe Double
doubleMay d | Right (dec,_) <- double d = Just dec
doubleMay _ = Nothing

removeSpaces = T.filter (/= ' ')
