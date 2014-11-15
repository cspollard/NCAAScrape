{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Basketball.Event where

import Data.Attoparsec.Text
import Control.Applicative
import Data.Text (Text)

import Data.NCAA.Score
import Data.NCAA.Time

import Data.NCAA.Basketball.Play


type Name = Text

data PeriodType = Period Int | Overtime Int deriving Show

data TimeoutType = TVTimeout
                 | ShortTimeout
                 | FullTimeout deriving Show


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
