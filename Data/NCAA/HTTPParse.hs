{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.HTTPParse where

import Control.Applicative
import Control.Monad (join)

import Data.Text
import Data.Attoparsec.Text
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

import Data.Time.Calendar



-- retrieve the page at a given URL
readURL :: String -> IO Text
readURL u = pack <$>
                join (getResponseBody <$> simpleHTTP (getRequest u))


scoreboardBaseURL :: String
scoreboardBaseURL = "http://www.ncaa.com/scoreboard/basketball-men/d1/"

pbpBaseURL :: String
pbpBaseURL = "http://data.ncaa.com/sites/default/files/data" 


getPlayByPlay :: String -> IO Text
getPlayByPlay s = readURL $ pbpBaseURL ++ s ++ "/pbp.json"


-- TODO
-- rewrite?
getPlayByPlays :: Day -> IO [Text]
getPlayByPlays day = do
                    let (y, m, d) = toGregorian day
                    let ymstr = show y ++ "/" ++ show m ++ "/"
                    -- day string needs to be zero padded.
                    let datestr = ymstr ++ (if d > 9 then show d else "0" ++ show d)
                    let url = scoreboardBaseURL ++ datestr

                    pbpurls <- parseOnly gameURLs <$> readURL url

                    case pbpurls of
                        Right urls -> sequence $ fmap (getPlayByPlay . unpack) urls
                        Left _ -> return []


gameURL :: Parser Text
gameURL = string "<a href=\"" *> takeTill (== '"') <*
                string "\" class=\"gamecenter\">"

nextURL :: Parser Text
-- make sure to eat up the '<' before continuing...
nextURL = takeTill (== '<') *> (gameURL <|> char '<' *> nextURL)

gameURLs :: Parser [Text]
gameURLs = many nextURL
