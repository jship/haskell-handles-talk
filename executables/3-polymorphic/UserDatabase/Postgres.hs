{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module UserDatabase.Postgres
  ( module UserDatabase -- Re-export the Handle type, constructor, and fields
  , Config(..)
  , new
  , withHandle
  ) where

import Data.Aeson ((.:))
import Database.PostgreSQL.Config (PGPool, PostgresConf)
import Types (Email, User(User)) -- just some dummy types
import UserDatabase (Handle(Handle, close, createUser, deleteUser))
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Config as PostgreSQL.Config
import qualified Logger

data Config = Config
  { postgresConf :: PostgresConf
  }

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "FromJSON UserDatabase.Postgres.Config" $ \obj ->
    fmap Config (obj .: "database")

new :: Config -> Logger.Handle -> IO Handle
new Config { postgresConf } logger = do
  connectionPool <- PostgreSQL.Config.createPGPool postgresConf
  pure Handle
    { createUser = createUserWith connectionPool logger
    , deleteUser = deleteUserWith connectionPool logger

    , close = pure ()
    }

createUserWith :: PGPool -> Logger.Handle -> Email -> IO User
createUserWith _connectionPool _logger _email = do
  -- actually make the user in postgres!
  pure User

deleteUserWith :: PGPool -> Logger.Handle -> Email -> IO ()
deleteUserWith _connectionPool _logger _email = do
  -- actually delete the user in postgres!
  pure ()

withHandle :: Config -> Logger.Handle -> (Handle -> IO a) -> IO a
withHandle config logger = Exception.bracket (new config logger) close
