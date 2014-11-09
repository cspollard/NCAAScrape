{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Team where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Control.Applicative

data Team = Team {
    home :: Bool,
    ident :: Int,
    seoName :: String,
    abbr :: String,
    shortName :: String,
    nickName :: String,
    color :: Int
} deriving Show


instance FromJSON Team where
    parseJSON (Object v) = Team <$>
        v .: "homeTeam" <*>
        v .: "id" <*>
        v .: "seoName" <*>
        v .: "sixCharAbbr" <*>
        v .: "shortName" <*>
        v .: "nickName" <*>
        v .: "color"

    parseJSON v = typeMismatch "failed to parse team." v


instance Eq Team where
    t1 == t2 = ident t1 == ident t2

instance Ord Team where
    compare t1 t2 = compare (ident t1) (ident t2)

