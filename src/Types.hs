{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types
    ( UserT (..),
      User,
      users
    )
  where

import GHC.Generics
import Data.Aeson
import Database.Beam

data UserT f = User
  { _userId        :: Columnar f Int
  , _userFirstName :: Columnar f String
  , _userLastName  :: Columnar f String
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

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
