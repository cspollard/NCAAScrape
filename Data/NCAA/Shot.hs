{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Shot where

import Data.Attoparsec.Text
import Control.Applicative

type Distance = Int

distance :: Parser Distance
distance = (string "from " *> decimal <* " feet out") <|>
            return 0

type Points = Int

data FreeThrowType = OneAndOne Int
                   | MultiFT Int Int
                   deriving Show

oneAndOne :: Parser FreeThrowType
oneAndOne = OneAndOne <$>
                    (return 1 <* string "1st" <|> return 2 <* string "2nd") <*
                    string " of 1-and-1"

multiFT :: Parser FreeThrowType
multiFT = MultiFT <$> (decimal <* string " of ") <*> decimal

freeThrowType :: Parser FreeThrowType
freeThrowType = (oneAndOne <|> multiFT) <?> "freeThrowType"


data ShotType = JumpShot
              | Layup
              | HookShot
              | Dunk
              | TipShot
              deriving Show

jumpShot :: Parser ShotType
jumpShot = return JumpShot <* string "jump"

layup :: Parser ShotType
layup = return Layup <* string "layup"

hookShot :: Parser ShotType
hookShot = return HookShot <* string "hook"

dunk :: Parser ShotType
dunk = return Dunk <* string "dunk"

tipShot :: Parser ShotType
tipShot = return TipShot <* string "tip"

shotType :: Parser ShotType
shotType = (choice [jumpShot, layup, hookShot, dunk, tipShot] <* string " shot") <?>
            "shotType"


data Shot = TwoPointer ShotType Distance
          | ThreePointer ShotType Distance
          | FreeThrow FreeThrowType
          deriving Show

threePointer :: Parser Shot
threePointer = ThreePointer <$>
                (string "a 3-point " *> shotType) <*>
                (skipSpace *> distance)

twoPointer :: Parser Shot
twoPointer = TwoPointer <$>
                (string "a " *> shotType) <*>
                (skipSpace *> distance)

freeThrow :: Parser Shot
freeThrow = FreeThrow <$> (string "free throw " *> freeThrowType)

shot :: Parser Shot
shot = choice [threePointer, twoPointer, freeThrow] <?> "shot"
