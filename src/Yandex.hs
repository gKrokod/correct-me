module Yandex (checkError) where

import Control.Monad
import Control.Exception
import Network.HTTP.Simple

import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)

import Data.Aeson (eitherDecode)
import Schema (SpellResult)


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
      let body = eitherDecode @SpellResult $ getResponseBody response
      case body of
        Right _ -> putStrLn $ show body  --L.writeFile "cfg/data.cfg" (getResponseBody response)
        Left _ -> putStrLn $ "error"
      -- let b' = eitherDecode body :: Either String [SpellResult]
      print body
  putStrLn "Hello bro"

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
-- txt2 = "Some people, when confronted with a problem, think, \"I know, I'll use threads,\" and then two the hav erpoblesms."

buildGetRequest :: Text -> Request
buildGetRequest txt =
  setRequestHost "speller.yandex.net" $
    setRequestMethod "GET" $
      setRequestSecure True $
        setRequestQueryString [("text", Just . encodeUtf8 $ txt)] $
          setRequestPath "/services/spellservice.json/checkText" $
            setRequestPort 443
              defaultRequest
