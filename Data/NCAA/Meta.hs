{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Meta where

import Control.Applicative
import Data.Aeson

import Data.NCAA.Division
import Data.NCAA.Status
import Data.NCAA.Team

data Meta = Meta {
    division :: Division,
    status :: Status,
    teams :: (Team, Team)
    }

instance Show Meta where
    show m = "Division: " ++ show (division m) ++ "\n" ++
                "Status: " ++ show (status m) ++ "\n" ++
                "Teams: " ++ show t1 ++ ", " ++ show t2
                where
                    (t1, t2) = teams m


instance FromJSON Meta where
    parseJSON = withObject "failed to parse meta."
                    (\o -> Meta <$>
                        o .: "division" <*>
                        o .: "status" <*>
                        o .: "teams"
                    )
