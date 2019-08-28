{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module UserDatabase
  ( Config(..)

  , Handle

  , new
  , close
  , withHandle

  , createUser
  , deleteUser
  ) where

import Data.Aeson ((.:))
import Database.PostgreSQL.Config (PGPool, PostgresConf)
import Types (User(User), Email)  -- just some dummy types
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Config as PostgreSQL.Config

data Config = Config
  { postgresConf :: PostgresConf
  -- ...  more static fields go here
  }

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "FromJSON UserDatabase.Config" $ \obj ->
    fmap Config (obj .: "database")

data Handle = Handle
  { connectionPool :: PGPool
  -- ...  more state-y and static fields go here
  }

new :: Config -> IO Handle
new Config { postgresConf } = do
  connectionPool <- PostgreSQL.Config.createPGPool postgresConf
  pure Handle
    { connectionPool
    }

close :: Handle -> IO ()
close = const (pure ())  -- can rely on GC in this case

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config = Exception.bracket (new config) close

createUser :: Handle -> Email -> IO User
createUser _handle _email = do
  -- actually make the user in postgres!
  pure User

deleteUser :: Handle -> Email -> IO ()
deleteUser _handle _email = do
  -- actually delete the user from postgres!
  pure ()
