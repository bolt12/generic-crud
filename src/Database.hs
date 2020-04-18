{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Database where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Schema.Tables
import Database.Beam.Backend.Types
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

-- dbChecked :: CheckedDatabaseSettings be UserDb
-- dbChecked = defaultMigratableDbSettings

selectAllE ::
           ( MonadBeam be m,
             Database be db,
             Beamable table,
             HasQBuilder be,
             Generic (table Identity),
             Generic (table Exposed),
             FromBackendRow be (table Identity)
           )
           => DatabaseEntity be db (TableEntity table) -> m [table Identity]
selectAllE e = runSelectReturningList . select $ all_ e

selectEById ::
            ( MonadBeam be m,
              Database be db,
              Beamable table,
              HasQBuilder be,
              Table table,
              Generic (table Identity),
              Generic (table Exposed),
              Generic (PrimaryKey table (WithConstraint (HasSqlEqualityCheck be))),
              Generic (PrimaryKey table Identity),
              Generic (PrimaryKey table Exposed),
              FromBackendRow be (table Identity),
              FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey table),
              FieldsFulfillConstraint (HasSqlValueSyntax
                (Sql92ExpressionValueSyntax
                  (Sql92SelectTableExpressionSyntax
                    (Sql92SelectSelectTableSyntax
                      (Sql92SelectSyntax (BeamSqlBackendSyntax be))))))
                (PrimaryKey table),
              Generic (PrimaryKey table (WithConstraint
                (HasSqlValueSyntax
                  (Sql92ExpressionValueSyntax
                    (Sql92SelectTableExpressionSyntax
                      (Sql92SelectSelectTableSyntax
                        (Sql92SelectSyntax
                          (BeamSqlBackendSyntax be))))))))
            )
            => PrimaryKey table Identity -> DatabaseEntity be db (TableEntity table) -> m (Maybe (table Identity))
selectEById id e = runSelectReturningOne . select $ do
  re <- all_ e
  guard_ (primaryKey re ==. val_ id)
  return re

insertE ::
        ( MonadBeam be m,
          Database be db,
          Beamable table,
          HasQBuilder be,
          Generic (table Identity),
          Generic (table Exposed),
          Generic
           (table (WithConstraint
             (HasSqlValueSyntax
               (Sql92ExpressionValueSyntax
                 (Sql92SelectTableExpressionSyntax
                   (Sql92SelectSelectTableSyntax
                     (Sql92SelectSyntax (BeamSqlBackendSyntax be)))))))),

          FieldsFulfillConstraint (HasSqlValueSyntax
            (Sql92ExpressionValueSyntax
              (Sql92SelectTableExpressionSyntax
                (Sql92SelectSelectTableSyntax
                  (Sql92SelectSyntax (BeamSqlBackendSyntax be))))))
            table,
          FromBackendRow be (table Identity)
        )
        => table Identity -> DatabaseEntity be db (TableEntity table) -> m (table Identity)
insertE item e = do
  runInsert . insert e $
    insertExpressions [val_ item]
  return item

updateE ::
        ( MonadBeam be m,
          Database be db,
          Beamable table,
          HasQBuilder be,
          Table table,
          Generic (table Identity),
          Generic (table Exposed),
          Generic (PrimaryKey table (WithConstraint (HasSqlEqualityCheck be))),
          Generic (PrimaryKey table Identity),
          Generic (PrimaryKey table Exposed),
          FromBackendRow be (table Identity),
          FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey table),
          FieldsFulfillConstraint (HasSqlValueSyntax
            (Sql92ExpressionValueSyntax
              (Sql92SelectTableExpressionSyntax
                (Sql92SelectSelectTableSyntax
                  (Sql92SelectSyntax (BeamSqlBackendSyntax be))))))
            (PrimaryKey table),
          FieldsFulfillConstraint (HasSqlValueSyntax
            (Sql92ExpressionValueSyntax
              (Sql92SelectTableExpressionSyntax
                (Sql92SelectSelectTableSyntax
                  (Sql92SelectSyntax (BeamSqlBackendSyntax be))))))
            table,
          Generic (PrimaryKey table (WithConstraint
            (HasSqlValueSyntax
              (Sql92ExpressionValueSyntax
                (Sql92SelectTableExpressionSyntax
                  (Sql92SelectSelectTableSyntax
                    (Sql92SelectSyntax
                      (BeamSqlBackendSyntax be)))))))),
          Generic (table (WithConstraint
            (HasSqlValueSyntax
              (Sql92ExpressionValueSyntax
                (Sql92SelectTableExpressionSyntax
                  (Sql92SelectSelectTableSyntax
                    (Sql92SelectSyntax
                      (BeamSqlBackendSyntax be))))))))
        )
        => PrimaryKey table Identity -> table Identity -> DatabaseEntity be db (TableEntity table) -> m (Maybe (table Identity))
updateE id new e = do
  re <- selectEById id e
  case re of
    Nothing -> return Nothing
    Just u  -> do
      runUpdate $ save e new
      return re

deleteE ::
        ( MonadBeam be m,
          Database be db,
          Beamable table,
          HasQBuilder be,
          Table table,
          Generic (table Identity),
          Generic (table Exposed),
          Generic (PrimaryKey table (WithConstraint (HasSqlEqualityCheck be))),
          Generic (PrimaryKey table Identity),
          Generic (PrimaryKey table Exposed),
          FromBackendRow be (table Identity),
          FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey table),
          FieldsFulfillConstraint (HasSqlValueSyntax
            (Sql92ExpressionValueSyntax
              (Sql92SelectTableExpressionSyntax
                (Sql92SelectSelectTableSyntax
                  (Sql92SelectSyntax (BeamSqlBackendSyntax be))))))
            (PrimaryKey table),
          Generic (PrimaryKey table (WithConstraint
            (HasSqlValueSyntax
              (Sql92ExpressionValueSyntax
                (Sql92SelectTableExpressionSyntax
                  (Sql92SelectSelectTableSyntax
                    (Sql92SelectSyntax
                      (BeamSqlBackendSyntax be))))))))
        )
        => PrimaryKey table Identity -> DatabaseEntity be db (TableEntity table) -> m (Maybe (table Identity))
deleteE id e = do
  re <- selectEById id e
  case re of
    Nothing -> return Nothing
    Just u -> do
      runDelete $ delete e (\p -> primaryKey p ==. val_ id)
      return re
