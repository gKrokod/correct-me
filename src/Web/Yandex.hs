{-# LANGUAGE RecordWildCards #-}
module Web.Yandex (revisionSpell, ConfigYandexService(..)) where

import Network.HTTP.Simple
import Data.Text.Encoding (encodeUtf8)
import Data.Text as T (Text, pack)
import Data.Aeson (eitherDecode)
import Web.Types (SpellResult)
import Control.Exception (try, SomeException, displayException)
import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

data ConfigYandexService = MkConfigYandexService
  { cHost :: Text,
    cPath :: Text,
    cPort :: Int,
    cMethod :: Text,
    cSecure :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

revisionSpell :: ConfigYandexService -> Text -> IO (Either Text SpellResult)
revisionSpell cfg txt = do
  response' <- try @SomeException . httpLBS . buildGetRequest cfg $ txt
  case response' of
    Left e -> pure . Left . T.pack . displayException $  e 
    Right response -> do
      let status = getResponseStatusCode response
      let body = eitherDecode @SpellResult $ getResponseBody response
      case (status , body) of
        (404, _) -> pure $ Left "spellservice 404"
        (301, _) -> pure $ Left "spellservice 301"
        (_, Right spellresult) -> pure $ Right spellresult
        _ -> pure $ Left $ "error:" <> T.pack (show status) <> T.pack (show body)

-- JSON-интерфейс:
-- https://speller.yandex.net/services/spellservice.json/checkText?text=синхрафазатрон+в+дубне
--
buildGetRequest :: ConfigYandexService -> Text -> Request
buildGetRequest (MkConfigYandexService {..}) txt =
  setRequestHost (encodeUtf8 cHost) $
    setRequestMethod (encodeUtf8 cMethod) $
      setRequestSecure cSecure $
        setRequestQueryString [("text", Just . encodeUtf8 $ txt)] $
          setRequestPath (encodeUtf8 cPath) $
            setRequestPort cPort
              defaultRequest
