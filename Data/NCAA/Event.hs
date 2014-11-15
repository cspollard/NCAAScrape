{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Basketball.Event where

import Data.Attoparsec.Text
import Data.Aeson ((.:), withObject, FromJSON(..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Control.Applicative
import Data.Text (Text)

import Data.NCAA.Score
import Data.NCAA.Time
import Data.NCAA.Parse

import Data.NCAA.Basketball.Play


data PeriodType = FirstHalf
                | SecondHalf
                | Overtime Int
                | Game
                deriving Show

half :: Parser PeriodType
half = return FirstHalf <* string "1st Half" <|>
        return SecondHalf <* string "2nd Half"

overtime :: Parser PeriodType
overtime = Overtime <$> decimal <* takeTill (== '.')

game :: Parser PeriodType
game = return Game <* string "End of the Game"

periodType :: Parser PeriodType
periodType = choice [half, overtime, game]


data TimeoutType = TVTimeout
                 -- TODO
                 -- which team took timeout?
                 | ShortTimeout
                 | FullTimeout
                 deriving Show

tvTimeout :: Parser TimeoutType
tvTimeout = return TVTimeout <* "Official TV timeout"

shortTimeout :: Parser TimeoutType
shortTimeout = return ShortTimeout <* manyTill anyChar (string "20-second timeout")

fullTimeout :: Parser TimeoutType
fullTimeout = return ShortTimeout <* manyTill anyChar (string "full timeout")

timeoutType :: Parser TimeoutType
timeoutType = choice [tvTimeout, shortTimeout, fullTimeout]


data Event = PeriodEnd PeriodType
           | Timeout TimeoutType
           -- put score second so we can parse event JSON objects quickly
           | Play Time Score PlayType
           deriving Show


periodEnd :: Parser Event
periodEnd = PeriodEnd <$> periodType

timeout :: Parser Event
timeout = Timeout <$> timeoutType


-- TODO
-- this very badly needs to be cleaned up.
toEvent :: A.Object -> A.Parser Event
toEvent o = -- check plays first: they are most common
            (flip Play <$>
                o .: "score" <*>
                o .: "time" <*> 
                ((parseText playType =<< ht) <|> (parseText playType =<< vt))
            ) <|>

            -- periods always end in homeText.
            (parseText periodEnd =<< ht) <|>

            -- timeouts show up in both homeText and
            -- visitorText.
            (parseText timeout =<< ht) <|>
            (parseText timeout =<< vt)

            where
                ht = o .: "homeText"
                vt = o .: "visitorText"


instance FromJSON Event where
    parseJSON = withObject "failed to parse event." toEvent
