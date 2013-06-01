{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Finance.Forex.GoogleCalc (GoogQuery(..))  where

import           Finance.Forex.Types
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8   as B
import qualified Data.HashMap.Lazy       as HM
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Text.Read
import           Data.Time
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Header
import           System.Locale

data GoogQuery = GoogQuery T.Text T.Text

instance Query GoogQuery where
  url (GoogQuery base counter)
    = concat ["http://www.google.com/ig/calculator?hl=en&q=1"
             ,T.unpack base
             ,"%3D%3F"
             ,T.unpack counter]
  respHandler (GoogQuery base counter) response
    | (Just rawtime) <- lookup hDate . responseHeaders $ response
    , (Just time) <- parseTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" $ B.unpack rawtime
    = do let parsedValue = decode . TLE.encodeUtf8 . quoteJSONKeys . TLE.decodeUtf8 . responseBody $ response
             addCurrency r = r {pair = T.append base counter}
         Quote <$> (pure time)
               <*> (replicate 1 <$> addCurrency <$> parsedValue)
  respHandler _  _ = mzero

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
