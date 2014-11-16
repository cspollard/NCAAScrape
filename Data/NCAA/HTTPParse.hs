{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.HTTPParse where

import Control.Applicative
import Control.Monad (join)

import Data.Text
import Data.Attoparsec.Text
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

import Data.Time.Calendar


scoreboardBaseURL = "http://www.ncaa.com/scoreboard/basketball-men/d1/"


getPlayByPlay :: String -> IO Text
getPlayByPlay s = pack <$>
                    readURL ("http://data.ncaa.com/sites/default/files/data" ++ s ++ "pbp.json")


-- HERE

getPlayByPlays :: Day -> IO [Text]
getPlayByPlays day =
                    where
                        (y, m, d) = toGregorian day
                        url = scoreboardBaseURL ++
                            show (y `mod` 100) ++ "/" ++ show m ++ "/" ++ show d
                        pbps = 


gameURLs :: Parser [Text]
gameURLs = many (string "<a href=\"" *> takeTill (== '"') <*
                string "\" class=\"gamecenter\">")


readURL :: String -> IO String
readURL u = join $ getResponseBody <$> simpleHTTP (getRequest u)
