{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Score where

import Data.Aeson
import Data.Attoparsec.Text (decimal, char)
import Data.NCAA.Parse
import Control.Applicative

data Score = Score {
    home :: Int,
    away :: Int
} deriving (Read, Show)

instance FromJSON Score where
    parseJSON = parseText $
                    Score <$> (decimal <* char '-') <*> decimal
