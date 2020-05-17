{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Docs where

import Servant
import Servant.API.Generic
import Servant.Docs
import Types
import Server
import Database.Beam.Schema
import Data.Functor.Identity

instance ToCapture (Capture "id" primaryKey) where
  toCapture _ =
    DocCapture "id"
               "(primary key) unique identifier"

instance ToSample User where
  toSamples _ = singleSample (User 1 "Haskell" "Curry")

apiDocs :: API
apiDocs = docs @(ToServantApi (CRUDRoute "user" (PrimaryKey UserT Identity) User)) api

markdownDocs :: String
markdownDocs = markdown apiDocs
