{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Database where

import Database.Beam
import Database.Beam.Postgres
import Types

newtype UserDb f = UserDb
              { _shoppingCartUsers :: f (TableEntity UserT) }
                deriving (Generic, Database be)

userDb :: DatabaseSettings be UserDb
userDb = defaultDbSettings
