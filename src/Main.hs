{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Aeson
import Data.Aeson.Types
import Control.Monad (mzero)
import Data.Monoid
import qualified Data.ByteString.Lazy as B 
import qualified Data.Text as T
import qualified Data.Vector as V


data BklRooms = BklRooms {
  hotel :: T.Text,
  bookable :: T.Text,
  rooms :: [T.Text],
  availability :: Int,
  maxOccu :: Int
  } deriving Show

instance FromJSON BklRooms where
  parseJSON (Object v) = BklRooms <$>
                         v .: "hotel" <*>
                         v .: "bookable" <*>
                         v .: "rooms" <*>
                         v .: "availability" <*>
                         v .: "maxOccu"
  parseJSON _ = mzero                       
                         

instance ToJSON BklRooms where
  toJSON (BklRooms a b c d e) =
    object [ "hotel" .= a,
             "bookable" .= b,
             "rooms" .= c,
             "availability" .= d,
             "maxOccu" .= e ]
  toEncoding (BklRooms a b c d e) =
    pairs ("hotel" .= a <>
           "bookable" .= b <>
           "rooms" .= c <>
           "availability" .= d <>
           "maxOccu" .= e)
  



parseStringAttr :: T.Text -> Object -> Maybe T.Text
parseStringAttr attr obj = do
  val <- parseMaybe (.: attr) obj
  case val of
    String "null" -> Nothing
    String v -> Just v
    Null -> Nothing
    _ -> Nothing

parseArrayAttr :: T.Text -> Object -> [T.Text]
parseArrayAttr attr obj =   
  case parseMaybe (.: attr) obj of
    Just (Array arr) -> V.toList $ fmap (\(String x) -> x) arr
    Just Null -> []
    Just (String "null") -> []
    _ -> []

parseIntAttr :: T.Text -> Object -> Maybe Int
parseIntAttr attr obj = do
  val <- parseMaybe (.: attr) obj
  case val of
    Number v -> Just $ round v
    _ -> Nothing
    

parseBklRooms :: Object -> Maybe BklRooms
parseBklRooms obj = do
  hotelName <- parseStringAttr "propertyName" obj
  bklAv <- parseIntAttr "availableUnits" obj
  bkl <- parseMaybe (.: "bookable") obj
  bklData <- parseMaybe (.: "basicData") bkl
  bklSpecs <- parseMaybe (.: "bklSpecs") bklData
  let bklRooms = parseArrayAttr "roomIds" bklData  
  bklName <- parseStringAttr "name" bklSpecs
  bklOccu <- parseIntAttr "maxOccu" bklData
  return $ BklRooms hotelName bklName bklRooms bklAv bklOccu
  


main = do
  contents <- B.readFile "/home/sebastian/haskellAeson/bookables.json"
  case eitherDecode' contents of
    Left error -> print error
    Right json -> do
      let (Just bookables) = sequence $ fmap parseBklRooms $ V.toList json
      -- Note that since [] and BklRooms have ToJSON intances, [BklRooms] can
      -- be encoded naturally.
      B.writeFile "/home/sebastian/haskellAeson/output.json" (encode bookables)
      
      


