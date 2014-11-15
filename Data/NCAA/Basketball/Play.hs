{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Basketball.Play where

import Data.Attoparsec.Text
import Control.Applicative
import Data.Text (Text)

import Data.NCAA.Basketball.Player
import Data.NCAA.Basketball.Shot


data Assist = Assist Name deriving Show


data ReboundType = OffensiveRebound | DefensiveRebound deriving Show

reboundType :: Parser ReboundType
reboundType = return DefensiveRebound <* string "a defensive" <|>
                return OffensiveRebound <* string "an offensive"


data TurnoverType = LostBall | Travel deriving Show


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
          | Block Name
          -- TODO
          -- steals?
          | Turnover TurnoverType Name
          | Rebound ReboundType Name
          | Foul FoulType Name
          deriving Show


rebound :: Parser PlayType
rebound = flip Rebound <$>
            manyTill (string " with ") <*>
            reboundType <* string " rebound"

foul :: Parser PlayType
foul = Foul <$>
        foulType <*>
        " committed by " *> takeTill (char '.')

