{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Play where

import Data.Aeson
import Control.Applicative
import Data.Text (Text)

import Data.NCAA.Score
import Data.NCAA.Time


data Play = Play {
    score :: Maybe Score,
    time :: Time,
    visitorText :: Maybe Text,
    homeText :: Maybe Text
} deriving Show

instance FromJSON Play where
    parseJSON = withObject "failed to parse play."
                    (\o -> Play <$>
                        o .:? "score" <*>
                        o .: "time" <*>
                        o .: "visitorText" <*>
                        o .: "homeText"
                    )
