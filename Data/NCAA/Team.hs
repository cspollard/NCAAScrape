module Data.NCAA.Team where

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



instance Eq Team where
    t1 == t2 = teamID t1 == teamID t2

instance Ord Team where
    compare t1 t2 = compare (teamID t1) (teamID t2)

