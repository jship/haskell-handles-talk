module Main (main) where

import Types (Email(Email))
import qualified UserDatabase

main :: IO ()
main = do
  -- Read this in from a file or w/e
  let config = UserDatabase.Config { UserDatabase.postgresConf = undefined }

  UserDatabase.withHandle config $ \handle -> do
    let dummyEmail = Email
    _user <- UserDatabase.createUser handle dummyEmail
    UserDatabase.deleteUser handle dummyEmail
