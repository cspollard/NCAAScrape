{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Basketball.Play where

import Data.Attoparsec.Text
import Control.Applicative
import Data.Text (Text)

import Data.NCAA.Score
import Data.NCAA.Time

import Data.NCAA.Basketball.Shot


type Player = Text

data Assist = Assist Player deriving Show

data FoulType = OffensiveFoul
              | PersonalFoul
              | ShootingFoul
              deriving Show

data ReboundType = OffensiveRebound | DefensiveRebound deriving Show

data TurnoverType = LostBall | Travel deriving Show

data TimeoutType = TVTimeout
                 | ShortTimeout
                 | FullTimeout deriving Show

data PlayType = Make Shot Player (Maybe Assist)
              | Miss Shot Player
              | Block Player
              | Steal Player
              | Turnover TurnoverType Player
              -- TODO
              -- offensive and defensive
              | Rebound ReboundType Player
              | Foul FoulType Player
              | Timeout TimeoutType
              | PeriodEnd
              deriving Show


data Play = Play {
    score :: Maybe Score,
    time :: Time,
    play :: PlayType
} deriving Show

instance FromJSON Play where
    parseJSON = withObject "failed to parse play."
                    (\o -> Play <$>
                        (o .: "score" <|> return Nothing) <*>
                        o .: "time" <*>
                        o .: "visitorText" <*>
                        o .: "homeText"
                    )

