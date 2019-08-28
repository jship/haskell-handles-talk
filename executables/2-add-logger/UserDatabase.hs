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
import Types (User(User), Email) -- just some dummy types
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Config as PostgreSQL.Config
import qualified Logger

data Config = Config
  { postgresConf :: PostgresConf
  }

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "FromJSON UserDatabase.Config" $ \obj ->
    fmap Config (obj .: "database")

data Handle = Handle
  { connectionPool :: PGPool
  , logger :: Logger.Handle
  }

new :: Config -> Logger.Handle -> IO Handle
new Config { postgresConf } logger = do
  connectionPool <- PostgreSQL.Config.createPGPool postgresConf
  pure Handle
    { connectionPool
    , logger
    }

close :: Handle -> IO ()
close = const (pure ())  -- can rely on GC in this case

withHandle :: Config -> Logger.Handle -> (Handle -> IO a) -> IO a
withHandle config logger = Exception.bracket (new config logger) close

createUser :: Handle -> Email -> IO User
createUser Handle { logger } email = do
  Logger.log logger $ "Creating user w/ email " <> show email
  -- actually make the user in postgres!
  pure User

deleteUser :: Handle -> Email -> IO ()
deleteUser _handle _email = do
  -- actually delete the user from postgres!
  pure ()
