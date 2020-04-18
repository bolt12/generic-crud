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
import Database.Beam.Backend.Types
import Database.Beam.Schema.Tables
import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Postgres.Syntax
import GHC.Generics

data CRUDEff primaryKey entity m a where
  All :: CRUDEff primaryKey entity m [entity]
  Get :: primaryKey -> CRUDEff primaryKey entity m entity
  Post :: entity -> CRUDEff primaryKey entity m entity
  Put :: primaryKey -> entity -> CRUDEff primaryKey entity m entity
  Delete :: primaryKey -> CRUDEff primaryKey entity m entity

makeSem ''CRUDEff

data DbError = UserNotFound deriving Show

databaseEffBeamToIO ::
                    ( Members '[Embed IO, Error DbError, Input Connection, Trace] r,
                      Beamable table,
                      Table table,
                      Database Postgres db,
                      Show (table Identity),
                      Show (PrimaryKey table Identity),
                      Generic (table Exposed),
                      Generic (table Identity),
                      Generic (table (WithConstraint (HasSqlValueSyntax PgValueSyntax))),
                      Generic (PrimaryKey table Identity),
                      Generic (PrimaryKey table Exposed),
                      Generic (PrimaryKey table (WithConstraint (HasSqlEqualityCheck Postgres))),
                      Generic (PrimaryKey table (WithConstraint (HasSqlValueSyntax PgValueSyntax))),
                      FromBackendRow Postgres (table Identity),
                      FieldsFulfillConstraint (HasSqlEqualityCheck Postgres) table,
                      FieldsFulfillConstraint (HasSqlEqualityCheck Postgres) (PrimaryKey table),
                      FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) table,
                      FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) (PrimaryKey table)
                    )
                    => DatabaseEntity Postgres db (TableEntity table)
                    -> Sem (CRUDEff (PrimaryKey table Identity) (table Identity) ': r) a
                    -> Sem r a
databaseEffBeamToIO t sem = do
  conn <- input @Connection
  interpret (\case
    All -> do
      trace "Selecting all"
      embed . runBeamPostgres conn $ selectAllE t
    Get id -> do
      trace ("Selecting id:" ++ show id)
      r <- embed . runBeamPostgres conn $ selectEById id t
      case r of
        Nothing -> throw UserNotFound
        Just r_ -> return r_
    Post u -> do
      trace ("Inserting user" ++ show u)
      embed . runBeamPostgres conn $ insertE u t
    Put id u -> do
      trace ("Updating user: " ++ show id ++ " " ++ show u)
      r <- embed . runBeamPostgres conn $ updateE id u t
      case r of
        Nothing -> throw UserNotFound
        Just r_ -> return r_
    Delete id -> do
      trace ("Deleting user: " ++ show id)
      r <- embed . runBeamPostgres conn $ deleteE id t
      case r of
        Nothing -> throw UserNotFound
        Just r_ -> return r_
            )
            sem
