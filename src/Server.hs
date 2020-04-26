{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Server
    ( startApp
    , app
    ) where

import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import GHC.TypeLits
import Types
import Polysemy (Sem, Member, Embed, runM)
import Polysemy.Input
import Polysemy.Error
import Polysemy.Trace
import Control.Monad.Trans.Except
import Database.Beam
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate (migrationBackend)
import qualified DatabaseEff as Eff
import qualified Database as DB

data CRUDRoute resourceName primaryKey resourceType route = CRUDRoute
  { all :: route :- AppendSymbol resourceName "s" :> Get '[JSON] [resourceType],
    get :: route :- resourceName :> Capture "id" primaryKey :> Get '[JSON] resourceType,
    post :: route :- resourceName :> ReqBody '[JSON] resourceType :> Post '[JSON] resourceType,
    put :: route :- resourceName :> Capture "id" primaryKey :> ReqBody '[JSON] resourceType :> Put '[JSON] resourceType,
    delete :: route :- resourceName :> Capture "id" primaryKey :> Delete '[JSON] resourceType
  } deriving Generic

record :: Member (Eff.CRUDEff (PrimaryKey UserT Identity) (UserT Identity)) r 
       => CRUDRoute "user" (PrimaryKey UserT Identity) User (AsServerT (Sem r))
record = CRUDRoute
  { Server.all = Eff.all,
    get = Eff.get,
    post = Eff.post,
    put = Eff.put,
    Server.delete = Eff.delete
  }

app :: Connection -> Application
app conn = genericServeT interpretSem record
  where
    interpretSem :: Sem '[Eff.CRUDEff (PrimaryKey UserT Identity) (UserT Identity), Trace, Input Connection, Error Eff.DbError, Embed IO] a -> Handler a
    interpretSem = liftHandler
      . runM
      . runError @Eff.DbError
      . runInputConst conn
      . traceToIO
      . Eff.databaseEffBeamToIO (DB._users DB.userDb)
    liftHandler = Handler . ExceptT . fmap handleErrors
    handleErrors (Left Eff.UserNotFound) = Left err404 { errBody = "User not found" }
    handleErrors (Right r) = Right r

api :: Proxy (ToServantApi (CRUDRoute "user" (PrimaryKey UserT Identity) User))
api = genericApi (Proxy :: Proxy (CRUDRoute "user" (PrimaryKey UserT Identity) User))

startApp :: IO ()
startApp = do
  conn <- connect defaultConnectInfo
  runBeamPostgres conn (autoMigrate migrationBackend DB.dbChecked)
  run 8080 (app conn)

