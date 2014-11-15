{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Period where

import Data.Aeson
import Control.Applicative
import Data.Attoparsec.Text (decimal)
import Data.Text (Text)

import Data.NCAA.Event
import Data.NCAA.Parse


data Period = Period {
    number :: Int,
    display :: Text,
    events :: Events
} deriving Show


instance FromJSON Period where
    parseJSON = withObject "failed to parse period."
                (\o -> Period <$>
                    (o .: "periodNumber" >>= parseText decimal) <*>
                    o .: "periodDisplay" <*>
                    o .: "playStats"
                )
