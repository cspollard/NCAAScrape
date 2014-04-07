{-# LANGUAGE OverloadedStrings #-}

module NCAAGame where

import Control.Applicative
import Data.Aeson

import Debug.Trace (trace)
data NCAAData = NCAAData {
    dataMeta :: NCAAMeta,
    dataPeriods :: [NCAAPeriod]
    } deriving Show

instance FromJSON NCAAData where
    parseJSON (Object v) = trace "NCAAData" $ NCAAData <$>
        v .: "meta" <*>
        v .: "periods"


data NCAAMeta = NCAAMeta {
    metaTitle :: String,
    metaDescription :: String,
    metaDivision :: String,
    metaStatus :: String,
    metaPeriod :: String,
    metaMinutes :: String,
    metaSeconds :: String,
    metaTeams :: [NCAATeam]
    } deriving Show

instance FromJSON NCAAMeta where
    parseJSON (Object v) = trace "NCAAMeta" $ NCAAMeta <$>
        v .: "title" <*>
        v .: "description" <*>
        v .: "division" <*>
        v .: "status" <*>
        v .: "period" <*>
        v .: "minutes" <*>
        v .: "seconds" <*>
        v .: "teams"


data NCAATeam = NCAATeam {
    teamHomeTeam :: String,
    teamID :: String,
    teamSeoName :: String,
    teamSixCharAbbr :: String,
    teamShortName :: String,
    teamNickName :: String,
    teamColor :: String
    } deriving Show

instance FromJSON NCAATeam where
    parseJSON (Object v) = trace "NCAATeam" $ NCAATeam <$>
        v .: "homeTeam" <*>
        v .: "id" <*>
        v .: "seoName" <*>
        v .: "sixCharAbbr" <*>
        v .: "shortName" <*>
        v .: "nickName" <*>
        v .: "color"


data NCAAPeriod = NCAAPeriod {
    periodPeriodNumber :: String,
    periodPeriodDisplay :: String,
    periodPlayStats :: [NCAAPlayStats]
    } deriving Show

instance FromJSON NCAAPeriod where
    parseJSON (Object v) = trace "NCAAPeriod" $ NCAAPeriod <$>
        v .: "periodNumber" <*>
        v .: "periodDisplay" <*>
        v .: "playStats"


data NCAAPlayStats = NCAAPlayStats {
    playStatsScore :: String,
    playStatsTime :: String,
    playStatsVisitorText :: String,
    playStatsHomeText :: String}
    deriving Show

instance FromJSON NCAAPlayStats where
    parseJSON (Object v) = NCAAPlayStats <$>
        v .: "score" <*>
        v .: "time" <*>
        v .: "visitorText" <*>
        v .: "homeText"

{-
class FromValue a where
    fromValue :: Value -> a

data Team = Team {
    homeTeam :: Bool,
    teamID :: Int,
    teamSeoName :: String,
    teamSixCharAbbr :: String,
    teamShortName :: String,
    teamNickName :: String,
    teamColor :: String
    } deriving Show

instance FromValue Team where
    fromValue (Object v) = Team
        ((v ! "homeTeam") == "true")
        (read (v ! "id"))
        (v ! "seoName")
        (v ! "sixCharAbbr")
        (v ! "shortName")
        (v ! "nickName")
        (v ! "color")


type Score = (Int, Int)
type Time = Int -- number of seconds since start of period.

data Play = Play {
    playVisitorText :: String,
    playHomeText :: String,
    playScore :: Score,
    playTime :: Time
    } deriving Show

data Period = Period {
    periodNumber :: Int,
    periodPlays :: [Play]
    } deriving Show

type Division = String

data GameStatus = Final | Ongoing
    deriving Show

data Game = Game {
    gameStatus :: GameStatus,
    gameTeams :: (Team, Team),
    gameIsNeutral :: Bool,
    gamePeriods :: [Period]
    } deriving Show

instance Eq Team where
    t1 == t2 = teamID t1 == teamID t2

instance Ord Team where
    compare t1 t2 = compare (teamID t1) (teamID t2)



parseTime :: String -> Time
parseTime t = (read *** read) (splitBy ':' t)

splitBy' :: Eq a => a -> [a] -> [a] -> ([a], [a])
splitBy' _ ys [] = (reverse ys, [])
splitBy' x ys (z:zs) = if x == z
                        then (reverse ys, zs) 
                        else splitBy' x (z:ys) zs

splitBy :: Eq a => a -> [a] -> ([a], [a])
splitBy x = splitBy' x []
-}
