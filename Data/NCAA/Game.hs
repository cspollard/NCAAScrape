{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Game where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (typeMismatch)

import Data.HashMap.Strict ((!))

import Data.NCAA.Meta
import Data.NCAA.Period


data Game = Game {
    meta :: Meta,
    periods :: [Period]
    } deriving Show

instance FromJSON Game where
    parseJSON v@(Object o) = Game <$>
                        o .: "meta" <*>
                        o .: "periods"

    parseJSON v = typeMismatch "failed to parse game." v


