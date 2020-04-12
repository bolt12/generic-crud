{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Database where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate
import Database.Beam.Postgres.Syntax
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import GHC.Generics
import Types

newtype UserDb f = UserDb
              { _users :: f (TableEntity UserT) }
                deriving (Generic, Database be)

userDb :: DatabaseSettings Postgres UserDb
userDb = defaultDbSettings

selectAllE e = runSelectReturningList . select $ all_ e

selectEById id e = runSelectReturningOne . select $ do
  re <- all_ e
  guard_ (primaryKey re ==. val_ id)
  return re

insertE user e = do
  runInsert . insert e $
    insertExpressions [User default_ (val_ (_userFirstName user)) (val_ (_userLastName user))]
  return user

updateE id new e = do
  re <- selectEById id e
  case re of
    Nothing -> return Nothing
    Just u  -> do
      runUpdate $ save e new
      return re

deleteE id e = do
  re <- selectEById id e
  case re of
    Nothing -> return Nothing
    Just u -> do
      runDelete $ delete e (\p -> primaryKey p ==. val_ id)
      return re
