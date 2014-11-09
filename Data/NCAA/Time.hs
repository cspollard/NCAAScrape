{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Time where

import Data.Aeson
import Control.Applicative

import Data.Attoparsec.Text (decimal, char)

import Data.NCAA.Parse

data Time = Time {
    minutes :: Int,
    seconds :: Int
    } deriving (Read, Show, Eq, Ord)

instance FromJSON Time where
    parseJSON = parseText $
                    Time <$> (decimal <* char ':') <*> decimal
