{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Basketball.Foul where

import Data.Attoparsec.Text
import Control.Applicative
import Data.Text (Text)

import Data.NCAA.Score
import Data.NCAA.Time
import Data.NCAA.Basketball.Event

data FoulType = OffensiveFoul
              | PersonalFoul
              | ShootingFoul
              deriving Show

foulType :: Parser FoulType
foulType = return OffensiveFoul <* string "Offensive foul" <|>
            return PersonalFoul <* string "Personal foul" <|>
            return ShootingFoul <* string "Shooting foul"


foul :: Parser PlayType
foul = Foul <$>
        foulType <*>
        " committed by " *> takeTill (char '.')
