module Main (main) where

import Types (Email(Email))
import qualified Logger
import qualified UserDatabase

main :: IO ()
main = do
  -- Read these in from files or w/e
  let loggerConfig = Logger.Config { Logger.minLogLevel = undefined }
  let userDbConfig = UserDatabase.Config { UserDatabase.postgresConf = undefined }

  Logger.withHandle loggerConfig $ \logger ->
    UserDatabase.withHandle userDbConfig logger $ \userDatabase -> do
      let dummyEmail = Email
      _user <- UserDatabase.createUser userDatabase dummyEmail
      UserDatabase.deleteUser userDatabase dummyEmail
