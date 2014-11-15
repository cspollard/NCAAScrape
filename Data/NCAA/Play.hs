{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Play where

import Data.Attoparsec.Text
import Control.Applicative
import Data.Text (Text, pack)

import Data.NCAA.Player
import Data.NCAA.Shot


data Assist = Assist Name deriving Show

assist :: Parser Assist
assist = Assist <$>
            fmap pack (manyTill anyChar (string " with ")) <* "the assist"


data ReboundType = OffensiveRebound | DefensiveRebound deriving Show

reboundType :: Parser ReboundType
reboundType = return DefensiveRebound <* string "a defensive" <|>
                return OffensiveRebound <* string "an offensive"


data TurnoverType = LostBall | Traveling deriving Show

turnoverType :: Parser TurnoverType
turnoverType = return LostBall <* string "a lost ball turnover: Lost Ball" <|>
                return Traveling <* string "a traveling turnover: Traveling"


data FoulType = OffensiveFoul
              | PersonalFoul
              | ShootingFoul
              deriving Show

foulType :: Parser FoulType
foulType = return OffensiveFoul <* string "Offensive foul" <|>
            return PersonalFoul <* string "Personal foul" <|>
            return ShootingFoul <* string "Shooting foul"


data PlayType = Make Shot Name (Maybe Assist)
          | Miss Shot Name
          | Turnover TurnoverType Name
          | Rebound ReboundType Name
          | Foul FoulType Name
          deriving Show

make :: Parser PlayType
make = flip Make <$>
            fmap pack (manyTill anyChar (string " makes ")) <*>
            shot <*>
            (fmap Just (string ". " *> assist) <|> return Nothing)

miss :: Parser PlayType
miss = flip Miss <$>
            fmap pack (manyTill anyChar (string " misses ")) <*>
            shot

turnover :: Parser PlayType
turnover = flip Turnover <$>
            fmap pack (manyTill anyChar (string " with ")) <*>
            turnoverType

rebound :: Parser PlayType
rebound = flip Rebound <$>
            -- need to convert to Text from String :(
            fmap pack (manyTill anyChar (string " with ")) <*>
            reboundType <* string " rebound"

foul :: Parser PlayType
foul = Foul <$>
        foulType <*>
        (string " committed by " *> takeTill (== '.'))

-- TODO
-- this will be very slow since all these parsers begin with manyTill.
-- is there a way to make them fail faster?
playType :: Parser PlayType
playType = choice [make, miss, turnover, rebound, foul]
