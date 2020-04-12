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
import qualified DatabaseEff as DB
import Control.Monad.Trans.Except
import Database.Beam
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate (migrationBackend)

data CRUDRoute resourceKind resourceType route = CRUDRoute
  { all :: route :- AppendSymbol resourceKind "s" :> Get '[JSON] [resourceType],
    get :: route :- resourceKind :> Capture "id" Int :> Get '[JSON] resourceType,
    post :: route :- resourceKind :> ReqBody '[JSON] resourceType :> Post '[JSON] resourceType,
    put :: route :- resourceKind :> Capture "id" Int :> ReqBody '[JSON] resourceType :> Put '[JSON] resourceType,
    delete :: route :- resourceKind :> Capture "id" Int :> Delete '[JSON] resourceType
  } deriving Generic

record :: Member (DB.DatabaseEff User) r => CRUDRoute "user" User (AsServerT (Sem r))
record = CRUDRoute
  { Server.all = DB.all,
    get = DB.get,
    post = DB.post,
    put = DB.put,
    Server.delete = DB.delete
  }

app :: Connection -> Application
app conn = genericServeT interpretSem record
  where
    interpretSem :: Sem '[DB.DatabaseEff User, Trace, Input Connection, Error DB.DbError, Embed IO] a -> Handler a
    interpretSem = liftHandler
      . runM
      . runError @DB.DbError
      . runInputConst conn
      . traceToIO
      . DB.databaseEffUserBeamToIO
    liftHandler = Handler . ExceptT . fmap handleErrors
    handleErrors (Left DB.UserNotFound) = Left err404 { errBody = "User not found" }

api :: Proxy (ToServantApi (CRUDRoute "user" User))
api = genericApi (Proxy :: Proxy (CRUDRoute "user" User))

startApp :: IO ()
startApp = do
  conn <- connect defaultConnectInfo
  -- runBeamPostgres conn (createSchema _ checkedUserDb)
  run 8080 (app conn)

