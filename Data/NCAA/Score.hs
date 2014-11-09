{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Score where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Control.Applicative

data Score = Score {
    home :: Int,
    away :: Int
} deriving (Read, Show)

instance FromJSON Score where
    parseJSON v@(String s) = Score <$> h' <*> a'
                            where
                                (h, a) = T.break (== '-') s
                                h' = case decimal h of
                                    Right (x, _) -> return x
                                    _ -> typeMismatch "failed to evaluate score." v
                                a' = case decimal a of
                                    Right (x, _) -> return x
                                    _ -> typeMismatch "failed to evaluate score." v

    parseJSON v = typeMismatch "failed to evaluate score." v
