{-# LANGUAGE OverloadedStrings #-}

module Data.NCAA.Team where

import Data.Aeson
import Control.Applicative
import qualified Data.Attoparsec.Text as AT
import Data.Attoparsec.Text (decimal, hexadecimal, string, char)

import Data.NCAA.Parse

data Team = Team {
    home :: Bool,
    ident :: Int,
    seoName :: String,
    abbr :: String,
    shortName :: String,
    nickName :: String,
    color :: Int
    }


parserHome :: AT.Parser Bool
parserHome = (string "true" *> return True) <|>
                (string "false" *> return False)
    

instance Show Team where
    show = abbr


instance FromJSON Team where
    parseJSON = withObject "failed to parse team."
                   (\o -> Team <$>
                        (o .: "homeTeam" >>= parseText parserHome) <*>
                        (o .: "id" >>= parseText decimal) <*>
                        o .: "seoName" <*>
                        o .: "sixCharAbbr" <*>
                        o .: "shortName" <*>
                        o .: "nickName" <*>
                        (o .: "color" >>= parseText (char '#' *> hexadecimal))
                    )


instance Eq Team where
    t1 == t2 = ident t1 == ident t2

instance Ord Team where
    compare t1 t2 = compare (ident t1) (ident t2)
