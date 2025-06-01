module Web.Query (queryToFilter, headersToLoginAndPassword) where

import Data.Aeson (eitherDecodeStrict)
import qualified Data.ByteString as B
import Data.ByteString.Base64 as B64
import Data.CaseInsensitive (CI)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Web.Types (FilterBy (..), FilterFromWeb (..))

--
queryToFilter :: [(B.ByteString, Maybe B.ByteString)] -> Maybe FilterBy
queryToFilter = convertFromWeb . mapMaybe (\(x, y) -> if x == "filter" then y else Nothing)
  where
    convertFromWeb :: [B.ByteString] -> Maybe FilterBy
    convertFromWeb [xs] = case eitherDecodeStrict @FilterFromWeb xs of
      Right (FilterFromWeb (Just x)) -> Just x
      _ -> Nothing
    convertFromWeb _ = Nothing

headersToLoginAndPassword :: [(CI B.ByteString, B.ByteString)] -> Maybe (T.Text, T.Text)
headersToLoginAndPassword ((header, loginpass) : xs)
  | header == "Authorization"
      && B.take 6 loginpass == "Basic " =
      Just $ splitLoginPass (B.drop 6 loginpass)
  | otherwise = headersToLoginAndPassword xs
  where
    splitLoginPass :: B.ByteString -> (T.Text, T.Text)
    splitLoginPass xs' =
      let colon = fromIntegral $ fromEnum ':'
       in case map E.decodeUtf8 . B.splitWith (== colon) . B64.decodeBase64Lenient $ xs' of
            [l, p] -> (l, p)
            _ -> ("", "")
headersToLoginAndPassword [] = Nothing
