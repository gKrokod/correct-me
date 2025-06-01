{-# LANGUAGE RecordWildCards #-}

module Web.Yandex (revisionSpell, ConfigYandexService (..)) where

import Control.Exception (SomeException, displayException, try)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode)
import Data.Text as T (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Simple
import Web.Types (SpellResult)
import Control.Concurrent.Async (race)
import Control.Concurrent (threadDelay)

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
  timeout <- race (threadDelay 10000000) runYandexService 
  case timeout of
    Left _ -> pure $ Left "timeout"
    Right r -> pure r
  where runYandexService = do
          response' <- try @SomeException . httpLBS . buildGetRequest cfg $ txt
          case response' of
            Left e -> pure . Left . T.pack . displayException $ e
            Right response -> do
              let status = getResponseStatusCode response
              let body = eitherDecode @SpellResult $ getResponseBody response
              case (status, body) of
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
            setRequestPort
              cPort
              defaultRequest
