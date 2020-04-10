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
import qualified DatabaseEff as DB
import Control.Monad.Trans.Except

data CRUDRoute resourceKind resourceType route = CRUDRoute
  { all :: route :- AppendSymbol resourceKind "s" :> Get '[JSON] [resourceType],
    get :: route :- resourceKind :> Capture "id" Int :> Get '[JSON] resourceType,
    post :: route :- resourceKind :> ReqBody '[JSON] resourceType :> Post '[JSON] resourceType,
    put :: route :- resourceKind :> Capture "id" Int :> ReqBody '[JSON] resourceType :> Put '[JSON] resourceType,
    delete :: route :- resourceKind :> Capture "id" Int :> Delete '[JSON] resourceType
  } deriving Generic

record :: Member DB.DatabaseEff r => CRUDRoute "user" User (AsServerT (Sem r))
record = CRUDRoute
  { Server.all = DB.all,
    get = DB.get,
    post = DB.post,
    put = DB.put,
    delete = DB.delete
  }

app :: Application
app = genericServeT interpretSem record
  where
    interpretSem = liftHandler . runM . DB.databaseEffToIO
    liftHandler = Handler . ExceptT . fmap Right

api :: Proxy (ToServantApi (CRUDRoute "user" User))
api = genericApi (Proxy :: Proxy (CRUDRoute "user" User))

startApp :: IO ()
startApp = run 8080 app

