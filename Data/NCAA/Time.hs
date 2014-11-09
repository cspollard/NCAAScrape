{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Time where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Control.Applicative

data Time = Time {
    mins :: Int,
    secs :: Int
} deriving (Read, Show)

instance FromJSON Time where
    parseJSON v@(String s) = Time <$> minutes' <*> seconds'
                            where
                                (minutes, seconds) = T.break (== ':') s
                                minutes' = case decimal minutes of
                                    Right (x, _) -> return x
                                    _ -> typeMismatch "failed to evaluate score." v
                                seconds' = case decimal seconds of
                                    Right (x, _) -> return x
                                    _ -> typeMismatch "failed to evaluate score." v

    parseJSON v = typeMismatch "failed to evaluate time." v
