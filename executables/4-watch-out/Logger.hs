{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( Config(..)

  , Handle

  , new
  , close
  , withHandle

  , Level(..)
  , log
  ) where

import Data.Aeson ((.:))
import Prelude hiding (log)
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson

data Config = Config
  { minLogLevel :: Level
  }

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "FromJSON Logger.Config" $ \obj ->
    fmap Config (obj .: "minLogLevel")

data Handle = Handle

new :: Config -> IO Handle
new = const (pure Handle)

close :: Handle -> IO ()
close = mempty

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config = Exception.bracket (new config) close

data Level
  = Level'Debug
  | Level'Info
  | Level'Warn
  | Level'Error

instance Aeson.FromJSON Level where
  parseJSON = undefined

log :: Handle -> String -> IO ()
log = mempty
