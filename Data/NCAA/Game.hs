{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Game where

import Control.Applicative
import Data.Aeson

import Data.NCAA.Meta
import Data.NCAA.Period


data Game = Game {
    meta :: Meta,
    periods :: [Period]
    } deriving Show

instance FromJSON Game where
    parseJSON = withObject "failed to parse game."
                    (\o -> Game <$>
                        o .: "meta" <*>
                        o .: "periods"
                    )
