module Data.NCAA.Period where

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
