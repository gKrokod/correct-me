module Yandex where

import Control.Monad
import Control.Exception
import Network.HTTP.Simple

import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import GHC.Generics (Generic)


data SpellResult = MkSpellResult {
  code :: Int,
  pos :: Int,
  row :: Int,
  col :: Int,
  len :: Int,
  word :: Text,
  s :: [Text]
                               }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

  -- startApp
checkError :: IO ()
checkError = do
  --handler <<- loadConfig, DataBase, Setup another
  response' <- try . httpLBS .  buildGetRequest $ txt1
-- async можно тут сделать и время задать выполнения. 
  print response'
  case response' of
    Left e -> do
      print (e :: SomeException)
    Right response -> do
      let status = getResponseStatusCode response
      when (404 == status || status == 301) (putStrLn "Error! Bot Server 404 or 301")
      let body = eitherDecode @[SpellResult] $ getResponseBody response
      -- let b' = eitherDecode body :: Either String [SpellResult]
      print body
  print "Hello bro"

  -- run 8081 $ spellServer

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

txt1 :: Text
txt1 = "синхрафазатрон в дубне дубне"

buildGetRequest :: Text -> Request
buildGetRequest txt =
  setRequestHost "speller.yandex.net" $
    setRequestMethod "GET" $
      setRequestSecure True $
        setRequestQueryString [("text", Just . encodeUtf8 $ txt)] $
          setRequestPath "/services/spellservice.json/checkText" $
            setRequestPort 443
              defaultRequest
--
--
-- [
--    { "code": 1, "pos": 0, "row": 0, "col": 0, "len": 14,
--      "word": "синхрофазатрон",
--      "s": [ "синхрофазотрон" ]
--    },
--    { "code": 3, "pos": 17, "row": 0, "col": 17, "len": 5,
--      "word": "дубне",
--      "s": [ "Дубне" ]
--    }
-- ]

-- Элементы XML-схемы ответа:
--
--     SpellResult — корневой элемент;
--     error — информация об ошибке (может быть несколько или могут отсутствовать);
--     word — исходное слово;
--     s — подсказка (может быть несколько или могут отсутствовать).
--
-- Элемент <error> содержит следующие атрибуты:
--
--     code — код ошибки, см. Коды ошибок;
--     pos — позиция слова с ошибкой (отсчет от 0);
--     row — номер строки (отсчет от 0);
--     col — номер столбца (отсчет от 0);
--     len — длина слова с ошибкой.
--
--
-- --    eresponse <- try $ httpLBS "http://does-not-exist"
--
--     case eresponse of
--         Left e -> print (e :: HttpException)
--         Right response -> L8.putStrLn $ getResponseBody response
