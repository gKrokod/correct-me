module Web.Yandex (revisionSpell) where

import Network.HTTP.Simple
import Data.Text.Encoding (encodeUtf8)
import Data.Text as T (Text, pack)
import Data.Aeson (eitherDecode)
import Web.Types (SpellResult)
import Control.Exception (try, SomeException, displayException)

-- config тут еще надо
revisionSpell :: Text -> IO (Either Text SpellResult)
revisionSpell txt = do
  response' <- try @SomeException . httpLBS . buildGetRequest $ txt
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
buildGetRequest :: Text -> Request
buildGetRequest txt =
  setRequestHost "speller.yandex.net" $
    setRequestMethod "GET" $
      setRequestSecure True $
        setRequestQueryString [("text", Just . encodeUtf8 $ txt)] $
          setRequestPath "/services/spellservice.json/checkText" $
            setRequestPort 443
              defaultRequest
