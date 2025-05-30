module Web.Utils (getBody, response403, response400, response404, response200, response500, mkGoodResponse) where

import Data.Binary.Builder (Builder, fromByteString)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text.Encoding as E
import Network.HTTP.Types (badRequest400, forbidden403, internalServerError500, notFound404, status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request, Response, getRequestBodyChunk, responseBuilder)

getBody :: Request -> IO ByteString
getBody = getRequestBodyChunk

response200 :: Response
response200 = responseBuilder status200 [] "All ok. status 200\n"

response400 :: Text -> Response
response400 = responseBuilder badRequest400 [] . fromByteString . E.encodeUtf8

response403 :: Response
response403 = responseBuilder forbidden403 [] "Forbidden. status 403\n"

response404 :: Response
response404 = responseBuilder notFound404 [] "NotFound. status 404\n"

response500 :: Response
response500 = responseBuilder internalServerError500 [] "internalServerError. status 500\n"

mkGoodResponse :: Builder -> Response
mkGoodResponse = responseBuilder status200 []
