module Main (main) where

import Types (Email(Email))
import qualified Logger
import qualified UserDatabase.Postgres as UserDatabase

main :: IO ()
main = do
  -- Read these in from files or w/e
  let loggerConfig = Logger.Config { Logger.minLogLevel = undefined }
  let userDbConfig = UserDatabase.Config { UserDatabase.postgresConf = undefined }

  Logger.withHandle loggerConfig $ \logger -> do
    -- Uh-oh, we leaked the handle outside of the `withHandle` callback!
    userDatabase <- UserDatabase.withHandle userDbConfig logger pure
    let dummyEmail = Email
    UserDatabase.deleteUser userDatabase dummyEmail

  Logger.withHandle loggerConfig $ \logger -> do
    userDatabase <- UserDatabase.new userDbConfig logger
    UserDatabase.close userDatabase
    -- Whoops, we closed the handle a second time!
    UserDatabase.close userDatabase
