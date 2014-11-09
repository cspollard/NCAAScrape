{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Play where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Control.Applicative

import Data.NCAA.Score
import Data.NCAA.Time


data Play = Play {
    score :: Maybe Score,
    time :: Time,
    visitorText :: Maybe Text,
    homeText :: Maybe Text
} deriving Show

instance FromJSON Play where
    parseJSON (Object v) = Play <$>
        v .: "score" <*>
        v .: "time" <*>
        v .: "visitorText" <*>
        v .: "homeText"

    parseJSON v = typeMismatch "failed to evaluate play." v
