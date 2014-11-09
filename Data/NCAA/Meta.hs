{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Meta where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (typeMismatch)

import Data.NCAA.Division
import Data.NCAA.Status
import Data.NCAA.Team

data Meta = Meta {
    division :: Division,
    status :: Status,
    teams :: (Team, Team)
    } deriving Show


instance FromJSON Meta where
    parseJSON (Object o) = Meta <$>
                        o .: "division" <*>
                        o .: "status" <*>
                        o .: "teams"

    parseJSON v = typeMismatch "failed to parse meta." v
