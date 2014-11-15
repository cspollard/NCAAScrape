{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Basketball.Shot where

import Data.Attoparsec.Text
import Control.Applicative
import Data.Text (Text)

type Distance = Int

distance :: Parser Distance
distance = string "from " *> decimal <* " feet out"

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


data ShotType = JumpShot
              | Layup
              | HookShot
              deriving Show

jumpShot :: Parser ShotType
jumpShot = return JumpShot <* string "jump shot"

layup :: Parser ShotType
layup = return Layup <* string "layup shot"

hookShot :: Parser ShotType
hookShot = return HookShot <* string "hook shot"

shotType :: Parser ShotType
shotType = choice [jumpShot, layup, hookShot]


data Shot = TwoPointer ShotType Distance
          | ThreePointer ShotType Distance
          | FreeThrow FreeThrowType
          deriving Show


threePointer :: Parser Shot
threePointer = ThreePointer <$>
                (string "3-point " *> shotType) <*>
                (string " from " *> decimal <* string " feet out")

twoPointer :: Parser Shot
twoPointer = TwoPointer <$>
                shotType <*>
                (string " from " *> decimal <* string " feet out" <|> return 0)

freeThrow :: Parser Shot
freeThrow = FreeThrow <$> string "free throw " *> (oneAndOne <|> multiFT)

shot :: Parser Shot
shot = choice [threePointer, twoPointer, freeThrow]
