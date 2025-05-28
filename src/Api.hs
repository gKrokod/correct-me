{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Api (Spell(..), API, FilterBy(..)) where

import Servant
import Data.Text (toLower)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Schema 

type API = "spell" :> QueryParam "filterby" FilterBy :> Get '[JSON] [Spell]

