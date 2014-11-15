{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Game where

import Control.Applicative
import Data.Aeson

import Data.NCAA.Meta
import Data.NCAA.Period
import Data.NCAA.Event


data Game = Game {
    meta :: Meta,
    periods :: [Period]
    }

gameEvents :: Game -> [Event]
gameEvents g = concatMap id (map events $ periods g)

instance Show Game where
    show g = show (meta g) ++ "\n" ++
                foldr (\p ps -> show p ++ "\n" ++ ps) "" (periods g) ++ "\n"


instance FromJSON Game where
    parseJSON = withObject "failed to parse game."
                    (\o -> Game <$>
                        o .: "meta" <*>
                        o .: "periods"
                    )
