{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Basketball.Event where

import Data.Attoparsec.Text
import Control.Applicative
import Data.Text (Text)

import Data.NCAA.Score
import Data.NCAA.Time

import Data.NCAA.Basketball.Shot
import Data.NCAA.Basketball.Foul


type Name = Text

data Assist = Assist Name deriving Show

data ReboundType = OffensiveRebound | DefensiveRebound deriving Show

data TurnoverType = LostBall | Travel deriving Show

data PeriodType = Period Int | Overtime Int deriving Show

data TimeoutType = TVTimeout
                 | ShortTimeout
                 | FullTimeout deriving Show

data PlayType = Make Shot Name (Maybe Assist)
          | Miss Shot Name
          | Block Name
          | Steal Name
          | Turnover TurnoverType Name
          -- TODO
          -- offensive and defensive
          | Rebound ReboundType Name
          | Foul FoulType Name
          deriving Show


data Event = PeriodEnd PeriodType Score
           | TimeOut TimeoutType
           | Play Time PlayType Score
           deriving Show

{-
instance FromJSON Event where
    parseJSON = withObject "failed to parse play."
                    (\o -> Play <$>
                        (o .: "score" <|> return Nothing) <*>
                        o .: "time" <*>
                        o .: "visitorText" <*>
                        o .: "homeText"
                    )
-}
