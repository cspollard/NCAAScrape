{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Period where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Control.Applicative

import Data.NCAA.Play


data Period = Period {
    number :: Int,
    display :: String,
    plays :: [Play]
} deriving Show

instance FromJSON Period where
    parseJSON (Object v) = Period <$>
        v .: "periodNumber" <*>
        v .: "periodDisplay" <*>
        v .: "playStats"

    parseJSON v = typeMismatch "failed to parse period." v
