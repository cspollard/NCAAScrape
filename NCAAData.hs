{-# LANGUAGE OverloadedStrings #-}

module NCAAData where

import Control.Applicative
import Control.Arrow
import Data.Aeson

import Debug.Trace (trace)
data NCAAData = NCAAData {
    ncaaDataMeta :: NCAAMeta,
    ncaaDataPeriods :: [NCAAPeriod]
    } deriving Show

instance FromJSON NCAAData where
    parseJSON (Object v) = NCAAData <$>
        v .: "meta" <*>
        v .: "periods"


data NCAAMeta = NCAAMeta {
    ncaaMetaTitle :: String,
    ncaaMetaDescription :: String,
    ncaaMetaDivision :: String,
    ncaaMetaStatus :: String,
    ncaaMetaPeriod :: String,
    ncaaMetaMinutes :: String,
    ncaaMetaSeconds :: String,
    ncaaMetaTeams :: [NCAATeam]
    } deriving Show

instance FromJSON NCAAMeta where
    parseJSON (Object v) = NCAAMeta <$>
        v .: "title" <*>
        v .: "description" <*>
        v .: "division" <*>
        v .: "status" <*>
        v .: "period" <*>
        v .: "minutes" <*>
        v .: "seconds" <*>
        v .: "teams"


data NCAATeam = NCAATeam {
    ncaaTeamHomeTeam :: String,
    ncaaTeamID :: String,
    ncaaTeamSeoName :: String,
    ncaaTeamSixCharAbbr :: String,
    ncaaTeamShortName :: String,
    ncaaTeamNickName :: String,
    ncaaTeamColor :: String
    } deriving Show

instance FromJSON NCAATeam where
    parseJSON (Object v) = NCAATeam <$>
        v .: "homeTeam" <*>
        v .: "id" <*>
        v .: "seoName" <*>
        v .: "sixCharAbbr" <*>
        v .: "shortName" <*>
        v .: "nickName" <*>
        v .: "color"


data NCAAPeriod = NCAAPeriod {
    ncaaPeriodPeriodNumber :: String,
    ncaaPeriodPeriodDisplay :: String,
    ncaaPeriodPlayStats :: [NCAAPlayStats]
    } deriving Show

instance FromJSON NCAAPeriod where
    parseJSON (Object v) = NCAAPeriod <$>
        v .: "periodNumber" <*>
        v .: "periodDisplay" <*>
        v .: "playStats"


data NCAAPlayStats = NCAAPlayStats {
    ncaaPlayStatsScore :: String,
    ncaaPlayStatsTime :: String,
    ncaaPlayStatsVisitorText :: String,
    ncaaPlayStatsHomeText :: String}
    deriving Show

instance FromJSON NCAAPlayStats where
    parseJSON (Object v) = NCAAPlayStats <$>
        v .: "score" <*>
        v .: "time" <*>
        v .: "visitorText" <*>
        v .: "homeText"


data Team = Team {
    homeTeam :: Bool,
    teamID :: Int,
    teamSeoName :: String,
    teamSixCharAbbr :: String,
    teamShortName :: String,
    teamNickName :: String,
    teamColor :: String
    } deriving Show

teamFromNCAATeam :: NCAATeam -> Team
teamFromNCAATeam ncaateam = Team h id' seoname abbr short nick color
    where
        h = ncaaTeamHomeTeam ncaateam == "true"
        id' = read $ ncaaTeamID ncaateam
        seoname = ncaaTeamSeoName ncaateam
        abbr = ncaaTeamSixCharAbbr ncaateam
        short = ncaaTeamShortName ncaateam
        nick = ncaaTeamNickName ncaateam
        color = ncaaTeamColor ncaateam

type Score = Maybe (Int, Int)
type Time = Double -- number of seconds since start of period.

data Play = Play {
    playVisitorText :: Maybe String,
    playHomeText :: Maybe String,
    playScore :: Score,
    playTime :: Time
    } deriving Show


playFromNCAAPlayStats :: NCAAPlayStats -> Play
playFromNCAAPlayStats ncaaplaystats = Play vtext htext score t
    where
        vtext = let s = ncaaPlayStatsVisitorText ncaaplaystats in
            case s of
                [] -> Nothing
                _ -> Just s

        htext = let s = ncaaPlayStatsHomeText ncaaplaystats in
            case s of
                [] -> Nothing
                _ -> Just s

        score = parseScore $ ncaaPlayStatsScore ncaaplaystats
        t = parseTime $ ncaaPlayStatsTime ncaaplaystats

data Period = Period {
    periodNumber :: Int,
    periodDisplay :: String,
    periodPlays :: [Play]
    } deriving Show

periodFromNCAAPeriod :: NCAAPeriod -> Period
periodFromNCAAPeriod ncaaperiod = Period pn pd plays
    where
        pn = read $ ncaaPeriodPeriodNumber ncaaperiod
        pd = ncaaPeriodPeriodDisplay ncaaperiod
        plays = map playFromNCAAPlayStats $ ncaaPeriodPlayStats ncaaperiod


type Division = String

data GameStatus = Final | Ongoing
    deriving Show


data Game = Game {
    gameDivision :: String,
    gameStatus :: GameStatus,
    gameTeams :: (Team, Team),
    gameIsNeutral :: Bool,
    gamePeriods :: [Period]
    } deriving Show

gameFromNCAAData :: NCAAData -> Game
gameFromNCAAData ncaadata = Game d status (t1, t2) isneut ps
    where
        meta = ncaaDataMeta ncaadata
        d = ncaaMetaDivision meta
        status = case ncaaMetaStatus meta of
            "Final" -> Final
            _ -> Ongoing

        t1 : t2 : _ = map teamFromNCAATeam $ ncaaMetaTeams meta

        isneut = not $ homeTeam t1 || homeTeam t2
        ps = map periodFromNCAAPeriod $ ncaaDataPeriods ncaadata


instance Eq Team where
    t1 == t2 = teamID t1 == teamID t2

instance Ord Team where
    compare t1 t2 = compare (teamID t1) (teamID t2)



parseTime :: String -> Time
parseTime t = 60*minutes + secs
    where
        (minutes, secs) = (read *** read) (splitBy ':' t)

parseScore :: String -> Score
parseScore s = case t of
        ([], []) -> Nothing
        _ -> Just $ (read *** read) t
    where t = splitBy '-' s

splitBy' :: Eq a => a -> [a] -> [a] -> ([a], [a])
splitBy' _ ys [] = (reverse ys, [])
splitBy' x ys (z:zs) = if x == z
                        then (reverse ys, zs) 
                        else splitBy' x (z:ys) zs

splitBy :: Eq a => a -> [a] -> ([a], [a])
splitBy x = splitBy' x []
