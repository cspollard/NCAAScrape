{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Parse where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec.Text as AT


-- lift an attoparsec parser to a JSON parser
parseText :: AT.Parser a -> Value -> Parser a
parseText t = withText "Attoparsec" p
        where p text = case AT.parseOnly t text of
                        Right x -> return x
                        Left err -> fail $ "Attoparsec error: " ++ err
