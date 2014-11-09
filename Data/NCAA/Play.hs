module Data.NCAA.Play where


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

