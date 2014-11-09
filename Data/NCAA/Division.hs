{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Division where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)


data Division = D1 | D2 | D3 deriving (Eq, Ord, Read, Show)

instance FromJSON Division where
    parseJSON = withText "failed to parse division."
                    (\s -> case s of
                        "d1" -> return D1
                        "d2" -> return D2
                        "d3" -> return D3
                        _ -> typeMismatch "failed to parse division." (String s)
                    )
