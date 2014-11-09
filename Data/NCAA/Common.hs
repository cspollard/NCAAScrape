{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Common where

import Data.Aeson
import Data.Attoparsec
import Control.Applicative

bool :: Parser Bool
bool = (string "true" *> return True) <|> (string "false" *> return False)
