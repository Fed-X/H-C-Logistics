{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Data.Text as T
import Data.Text.Read
import Network.HTTP
import Text.XML.Light.Types
import Text.XML.Light.Proc
import Text.XML.Light.Input

data MarketStat = MarketStat { itemId       :: String
                             , buyOrder     :: MarketOrder
                             , sellOrder    :: MarketOrder
                             }
                  deriving Show

data MarketOrder = Buy  { avg    :: Float
                        }
                 | Sell { avg    :: Float
                        --, mini   :: Float
                        --, maxi   :: Float
                        }
                  deriving Show

main = do
  rawXML <- fetchMarketStats
  let marketStats = onlyElems $ elContent (elChildren (onlyElems (parseXML rawXML) !! 1) !! 0)
  print $ parseItems marketStats

parseItems :: [Element] -> [MarketStat]
parseItems xml = map parseItem xml

parseItem :: Element -> MarketStat
parseItem xml = MarketStat { itemId = itemId', buyOrder = buyOrder', sellOrder = sellOrder' }
  where
    Just itemId' = findAttr (QName "id" Nothing Nothing) xml

    buyElm = (elChildren xml) !! 0
    buyAvg = read $ strContent $ (elChildren buyElm) !! 1
    buyOrder' = Buy { avg = buyAvg }

    sellElm = (elChildren xml) !! 1
    sellAvg = read $ strContent $ (elChildren sellElm) !! 1
    sellOrder' = Sell { avg = sellAvg }

fetchMarketStats = simpleHTTP (getRequest url) >>= getResponseBody
  where
    url = "http://api.eve-central.com/api/marketstat?typeid=34&typeid=35&regionlimit=10000002"