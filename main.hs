{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Text.HandsomeSoup (openUrl)
-- import Control.Monad.Maybe (runMaybeT)
-- import Control.Monad (liftM)
-- import Data.Maybe (fromJust)
-- import Data.ByteString.Lazy.Char8 (pack)

import qualified Data.ByteString.Lazy as BSL (readFile)
import Data.Aeson (eitherDecode)
import Control.Applicative ((<$>))
import Data.NCAA.Game
import Data.NCAA.Meta
import Data.NCAA.Period

testhtml :: String
testhtml = "http://www.ncaa.com/game/basketball-men/d1/2014/01/29/arizona-stanford/play-by-play"
--testhtml = "http://www.google.com/search?q=egon+schiele"


testjson :: String
-- testjson = "http://data.ncaa.com/sites/default/files/data/game/basketball-men/d1/2014/01/29/arizona-stanford/pbp.json"
testjson = "examplepbp.json"

testperiod = "period.json"
testmeta = "meta.json"




main :: IO ()
main = do
    -- s <- runMaybeT $ openUrl testjson
    -- let w = liftM gameFromNCAAData $ (eitherDecode . pack . fromJust ) s

    -- g <- eitherDecode <$> BSL.readFile testjson :: IO (Either String Game)
    g <- eitherDecode <$> BSL.readFile testmeta :: IO (Either String Meta)
    print g
