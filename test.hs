import Text.HandsomeSoup (openUrl)
import Control.Monad.Maybe (runMaybeT)
import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack)
import Control.Applicative

testhtml :: String
testhtml = "http://www.ncaa.com/game/basketball-men/d1/2014/01/29/arizona-stanford/play-by-play"
--testhtml = "http://www.google.com/search?q=egon+schiele"


testjson :: String
testjson = "http://data.ncaa.com/sites/default/files/data/game/basketball-men/d1/2014/01/29/arizona-stanford/pbp.json"

main :: IO ()
main = do
    s <- runMaybeT $ openUrl testjson
    print $ (decode . pack) <$> s

    return ()
