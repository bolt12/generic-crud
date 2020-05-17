{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Typeable

instance Typeable primaryKey => ToCapture (Capture "id" primaryKey) where
  toCapture _ =
    DocCapture "id" $
               "(" ++ show (typeRep (Proxy :: Proxy primaryKey)) ++ ")" ++ " unique identifier"

instance ToSample User where
  toSamples _ = singleSample (User 1 "Haskell" "Curry")

apiDocs :: API
apiDocs = docs @(ToServantApi (CRUDRoute "user" (PrimaryKey UserT Identity) User)) api

markdownDocs :: String
markdownDocs = markdown apiDocs
