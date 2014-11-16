{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))

import qualified Data.ByteString.Lazy as BSL (readFile)
import Data.Aeson (eitherDecode)
import Data.Time.Calendar

import Data.NCAA.Game
import Data.NCAA.Score
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
    -- s <- runMaybeT $ openUrl testjson

    -- g <- eitherDecode <$> BSL.readFile testjson :: IO (Either String Game)
    -- print g

    -- let es = fmap gameEvents g
    -- print $ fmap (foldr (flip addEvent) (Score 0 0)) es

    -- g <- eitherDecode <$> BSL.readFile testplay :: IO (Either String Event)
    -- print g

    pbps <- getPlayByPlays (fromGregorian 2014 11 15)

    print pbps
