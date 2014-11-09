{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Status where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)


data Status = Ongoing | Final deriving (Eq, Ord, Read, Show)

instance FromJSON Status where
    parseJSON v@(String s) = case s of
                                "Ongoing" -> return Ongoing
                                "Final" -> return Final
                                _ -> typeMismatch "failed to parse status." v

    parseJSON v = typeMismatch "failed to parse status." v
