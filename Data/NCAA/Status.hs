{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Status where

import Control.Applicative
import Data.Aeson
import Data.Attoparsec.Text (string)

import Data.NCAA.Parse



data Status = Ongoing | Final deriving (Eq, Ord, Read, Show)

instance FromJSON Status where
    parseJSON = parseText $
                    string "Ongoing" *> return Ongoing <|>
                    string "Final" *> return Final
