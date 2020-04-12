{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module DatabaseEff where

import Polysemy
import Polysemy.Input
import Polysemy.Trace
import Polysemy.Error

import Database
import Types
import Database.Beam
import Database.Beam.Postgres
import GHC.Generics

data DatabaseEff e m a where
  All :: DatabaseEff e m [e]
  Get :: Int -> DatabaseEff e m e
  Post :: e -> DatabaseEff e m e
  Put :: Int -> e -> DatabaseEff e m e
  Delete :: Int -> DatabaseEff e m e

makeSem ''DatabaseEff

data DbError = UserNotFound

databaseEffUserBeamToIO :: (Members '[Embed IO, Error DbError, Input Connection, Trace] r)
                        => Sem (DatabaseEff User ': r) a -> Sem r a
databaseEffUserBeamToIO sem = do
  conn <- input @Connection
  interpret (\case
    All -> do
      trace "Selecting all"
      embed . runBeamPostgres conn $ selectAllE (_users userDb)
    Get id -> do
      trace ("Selecting id:" ++ show id)
      let u = User { _userId = id }
      r <- embed . runBeamPostgres conn $ selectEById (primaryKey u) (_users userDb)
      case r of
        Nothing -> throw UserNotFound
        Just r_ -> return r_
    Post u -> do
      trace ("Inserting user" ++ show u)
      embed . runBeamPostgres conn $ insertE u (_users userDb)
    Put id u -> do
      trace ("Updating user: " ++ show id ++ " " ++ show u)
      let u_ = User { _userId = id }
      r <- embed . runBeamPostgres conn $ updateE (primaryKey u_) u (_users userDb)
      case r of
        Nothing -> throw UserNotFound
        Just r_ -> return r_
    Delete id -> do
      trace ("Deleting user: " ++ show id)
      let u_ = User { _userId = id }
      r <- embed . runBeamPostgres conn $ deleteE (primaryKey u_) (_users userDb)
      case r of
        Nothing -> throw UserNotFound
        Just r_ -> return r_
            )
            sem
