module Data.NCAA.Game where

import Data.Aeson
import Data.NCAA.Period


data NCAAData = NCAAData {
    ncaaDataMeta :: NCAAMeta,
    ncaaDataPeriods :: [Period]
    } deriving Show

instance FromJSON NCAAData where
    parseJSON (Object v) = NCAAData <$>
        v .: "meta" <*>
        v .: "periods"


data Division = D1 | D2 | D3 deriving (Eq, Ord, Read, Show)

data Status = Ongoing | Final deriving (Eq, Ord, Read, Show)

type Period = Int

type Time = (Int, Int)

type Score = Maybe (Int, Int)


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

