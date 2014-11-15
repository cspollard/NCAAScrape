{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Event where

import Data.Attoparsec.Text
import Data.Aeson ((.:), withObject, FromJSON(..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Control.Applicative

import Data.NCAA.Score
import Data.NCAA.Time
import Data.NCAA.Parse

import Data.NCAA.Play
import Data.NCAA.Shot


data PeriodType = FirstHalf
                | SecondHalf
                | Overtime Int
                | GamePeriod
                deriving Show

half :: Parser PeriodType
half = return FirstHalf <* string "1st Half" <|>
        return SecondHalf <* string "2nd Half"

overtime :: Parser PeriodType
overtime = Overtime <$> decimal <* takeTill (== '.')

game :: Parser PeriodType
game = return GamePeriod <* string "Game"

periodType :: Parser PeriodType
periodType = choice [half, overtime, game] <?> "periodType"


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
timeoutType = choice [tvTimeout, shortTimeout, fullTimeout] <?> "timeoutType"


data Event = PeriodStart PeriodType
           | PeriodEnd PeriodType
           | Timeout TimeoutType
           -- no score reported if it doesn't change
           -- Bool identifies the home team
           | Play Time Bool PlayType (Maybe Score)
           deriving Show


periodStart :: Parser Event
periodStart = PeriodStart <$> (string "Start of the " *> periodType)

periodEnd :: Parser Event
periodEnd = PeriodEnd <$> (string "End of the " *> periodType)

timeout :: Parser Event
timeout = Timeout <$> timeoutType


-- TODO
-- this very badly needs to be cleaned up.
toEvent :: A.Object -> A.Parser Event
toEvent o = -- check plays first: they are most common
            -- home team play?
            (Play <$>
                o .: "time" <*> 
                return True <*>
                (parseText playType =<< ht) <*>
                (o .: "score" <|> return Nothing)
            ) <|>

            -- away team play?
            (Play <$>
                o .: "time" <*> 
                return False <*>
                (parseText playType =<< vt) <*>
                (o .: "score" <|> return Nothing)
            ) <|>

            -- periods always start/end in homeText.
            (parseText periodStart =<< ht) <|>
            (parseText periodEnd =<< ht) <|>

            -- timeouts show up in both homeText and
            -- visitorText.
            (parseText timeout =<< ht) <|>
            (parseText timeout =<< vt)

            where
                ht = o .: "homeText"
                vt = o .: "visitorText"


instance FromJSON Event where
    parseJSON v = withObject "failed to parse event." toEvent v <|> fail (show v)


addHome :: Score -> Int -> Score
addHome s i = Score (home s + i) (away s)

addAway :: Score -> Int -> Score
addAway s i = Score (home s) (away s + i)

addEvent :: Score -> Event -> Score
addEvent s e = case e of
                Play _ h (Make sh _ _) _ ->
                    let f = if h then addHome s else addAway s in
                    case sh of
                        FreeThrow _ -> f 1
                        TwoPointer _ _ -> f 2
                        ThreePointer _ _ -> f 3

                othwerwise -> s
