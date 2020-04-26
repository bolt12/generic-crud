{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
    ( UserT (..),
      User,
      users
    )
  where

import GHC.Generics
import Data.Aeson
import Database.Beam
import qualified Data.Text as T
import Data.Bifunctor
import Web.HttpApiData
import Text.Read

data UserT f = User
  { _userId        :: Columnar f Int
  , _userFirstName :: Columnar f T.Text
  , _userLastName  :: Columnar f T.Text
  } deriving (Generic, Beamable)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User
deriving instance ToJSON User
deriving instance FromJSON User

instance Table UserT where
   data PrimaryKey UserT f = UserId (Columnar f Int) deriving (Generic, Beamable)
   primaryKey = UserId . _userId

deriving instance Show (Columnar f Int) => Show (PrimaryKey UserT f)
deriving instance Read (Columnar f Int) => Read (PrimaryKey UserT f)

instance ToHttpApiData (PrimaryKey UserT Identity) where
  toUrlPiece = T.toLower . T.pack . show . fromPK
    where
      fromPK (UserId pk) = pk

instance FromHttpApiData (PrimaryKey UserT Identity) where
  parseUrlPiece = bimap T.pack UserId . readEither . T.unpack

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
