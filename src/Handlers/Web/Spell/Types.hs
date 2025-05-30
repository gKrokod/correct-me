module Handlers.Web.Spell.Types  where
import Data.Text (Text)
import Schema 
import Web.Types(Client, SpellResult)

data SpellInternal = SpellInternal
  { phrase :: Text,
    author :: Client,
    revision :: SpellResult
  }
-- data PhraseToWeb = PhraseToWeb
--   { phrase :: Text,
--     author :: Text,
--     revision :: SpellResult
--   }
--
-- data NewsEditInternal = NewsEditInternal
--   { titleEditNews :: Maybe Title,
--     authorEditNews :: Maybe Login,
--     labelEditNews :: Maybe Label,
--     contentEditNews :: Maybe Content,
--     imagesEditNews :: [Image],
--     isPublishEditNews :: Maybe Bool
--   }
--
-- data NewsOut = MkNewsOut
--   { nTitle :: Title,
--     nTime :: UTCTime,
--     nAuthor :: Name,
--     nCategories :: [Label],
--     nContent :: Content,
--     nImages :: [URI_Image],
--     nIsPublish :: Bool
--   }
