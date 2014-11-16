{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))

import Data.Aeson (eitherDecode)
import Data.Time.Calendar
import Data.Text.Encoding (encodeUtf32LE)
import Data.ByteString.Lazy (fromStrict)

import Data.NCAA.Event
import Data.NCAA.HTTPParse


testhtml :: String
testhtml = "http://www.ncaa.com/game/basketball-men/d1/2014/01/29/arizona-stanford/play-by-play"
--testhtml = "http://www.google.com/search?q=egon+schiele"


testjson :: String
-- testjson = "http://data.ncaa.com/sites/default/files/data/game/basketball-men/d1/2014/01/29/arizona-stanford/pbp.json"
testjson = "game.json"

testperiod = "period.json"
testplay = "play.json"




main :: IO ()
main = do

    pbps <- getPlayByPlays (fromGregorian 2014 11 15)

    let games = map (eitherDecode . fromStrict . encodeUtf32LE) pbps :: [Either String Event]

    print games
