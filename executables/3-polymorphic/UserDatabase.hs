{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module UserDatabase
  ( Handle(..)
  ) where

import Types (User, Email) -- just some dummy types

data Handle = Handle
  { createUser :: Email -> IO User
  , deleteUser :: Email -> IO ()

  , close :: IO ()
  }
