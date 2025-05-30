module Web.Yandex (revisionSpell) where

import Control.Monad
import Control.Exception
import Network.HTTP.Simple
import Data.Text.Encoding (encodeUtf8)
import Data.Text as T (Text, pack)
import Data.Aeson (eitherDecode)
import Web.Types (SpellResult)

-- config тут еще надо
revisionSpell :: Text -> IO (Either Text SpellResult)
revisionSpell txt = do
  response' <- try @SomeException . httpLBS . buildGetRequest $ txt
  case response' of
    Left e -> do
      pure $ Left "Can't connect to spellservice"
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
-- fetch :: Config -> Maybe LastMessage -> IO (Maybe Message)
-- fetch cfg lm = do
--   response' <- try $ httpLBS $ buildGetRequest (cfg {cOffset = maybe "-1" (T.pack . show . succ . giveId . mID) lm})
--   case response' of
--     Left e -> do
--       print (e :: SomeException)
--       threadDelay 1000000
--       fetch cfg lm
--     Right response -> do
--       let status = getResponseStatusCode response
--       when (404 == status || status == 301) (TIO.putStrLn "Error! Bot Server 404 or 301")
--       let msg = eitherDecode $ getResponseBody response -- messages : text, gif
--
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
