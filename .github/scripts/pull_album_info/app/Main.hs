{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Standard library imports
import           System.Environment    (getArgs, lookupEnv)

-- Third-party library imports
import           Control.Lens          ((^?))
import           Data.Aeson            (FromJSON (parseJSON), ToJSON,
                                        Value (Object), decodeStrict, encode,
                                        (.:))
import           Data.Aeson.Lens       (AsNumber (_Integer), key, nth)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           GHC.Generics          (Generic)
import           Network.HTTP.Simple   (Query, getResponseBody, httpBS,
                                        parseRequest_, setRequestHeader,
                                        setRequestQueryString)

-- Data type definitions
data MainRelease = MainRelease {
  released :: String,
  imageUrl :: String,
  labels   :: [String],
  uri      :: String
} deriving (Show, Eq, Generic)

instance ToJSON MainRelease

instance FromJSON MainRelease where
  parseJSON (Object v) = do
    uri <- v .: "uri"
    released <- v .: "released"
    images <- v .: "images"
    imageUrl <- case images of
        (img:_) -> img .: "resource_url"
        []      -> fail "No images found"
    labels <- v .: "labels" >>= traverse (.: "name")
    return MainRelease {
      uri = uri,
      released = released,
      imageUrl = imageUrl,
      labels = labels
    }

-- Helper functions
runDiscogsQuery :: Query -> String -> IO ByteString
runDiscogsQuery query url = do
  maybeKey <- lookupEnv "DISCOG_KEY"
  maybeSecret <- lookupEnv "DISCOG_SECRET"
  (key, secret) <- case (maybeKey, maybeSecret) of
    (Just k, Just s) -> return (k, s)
    _ -> error "Environment variables DISCOG_KEY and/or DISCOG_SECRET are not set"
  let request =
        setRequestQueryString query $
        setRequestHeader "Authorization" [BS.pack $ "Discogs key=" ++ key ++ ", secret=" ++ secret] $
        setRequestHeader "User-Agent" ["pull-album-info/1.0 (usefulalgorithm@gmail.com)"] $
        parseRequest_ url
  getResponseBody <$> httpBS request

getMasterReleaseId :: String -> String -> IO String
getMasterReleaseId artistName albumName = do
  let url = "https://api.discogs.com/database/search"
      query =
        [ ("artist", Just $ BS.pack artistName),
          ("release_title", Just $ BS.pack albumName),
          ("type", Just "master")
        ]
  body <- BS.unpack <$> runDiscogsQuery query url
  case body ^? key "results" . nth 0 . key "master_id" . _Integer  of
    Just masterId -> return $ show masterId
    Nothing       -> fail "Failed to extract master_id from the response"

getMainReleaseId :: String -> IO String
getMainReleaseId masterId = do
  let url = "https://api.discogs.com/masters/" ++ masterId
  body <- BS.unpack <$> runDiscogsQuery [] url
  case body ^? key "main_release" . _Integer of
    Just mainId -> return $ show mainId
    Nothing     -> fail "Failed to extract main_release from the response"

getMainRelease :: String -> IO MainRelease
getMainRelease releaseId = do
  let url = "https://api.discogs.com/releases/" ++ releaseId
  body <- runDiscogsQuery [] url
  case (decodeStrict body :: Maybe MainRelease) of
    Just release -> return release
    Nothing      -> fail "Cannot decode main release"

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    [artistName, albumName] -> do
      release <- getMasterReleaseId artistName albumName
             >>= getMainReleaseId
             >>= getMainRelease
      putStrLn $ BS.unpack $ BS.toStrict $ encode release
    _ -> putStrLn "Usage: pull_album_info <artist_name> <album_name>"
