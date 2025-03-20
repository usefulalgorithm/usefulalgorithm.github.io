{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

-- Standard library imports
import           Control.Exception     (SomeException, try)
import           System.Environment    (getArgs, getProgName, lookupEnv)
import           System.FilePath       (takeDirectory)

-- Third-party library imports
import           Control.Lens          (Identity (runIdentity), (^?))
import           Data.Aeson            (FromJSON (parseJSON), ToJSON,
                                        Value (Object), decodeStrict, (.:))
import           Data.Aeson.Key        (fromString)
import           Data.Aeson.Lens       (AsNumber (_Integer), key, nth)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.List             as L (intercalate)
import           Data.Text             as T (unpack)
import           GHC.Generics          (Generic)
import           Network.HTTP.Simple   (Query, getResponseBody, httpBS,
                                        parseRequest_, setRequestHeader,
                                        setRequestQueryString)
import           Text.Ginger           (IncludeResolver, SourcePos, Template,
                                        ToGVal (..), dict, easyRender,
                                        parseGinger)

-- Data type definitions
data Release = Release {
  artists  :: [String],
  title    :: String,
  year     :: Int,
  released :: String,
  imageUrl :: String,
  labels   :: [String],
  uri      :: String
} deriving (Show, Eq, Generic)

instance ToJSON Release

instance ToGVal m Release where
  toGVal release = dict [
      ("artists", toGVal . L.intercalate ", " . artists $ release),
      ("title", toGVal $ title release),
      ("year", toGVal $ year release),
      ("released", toGVal $ released release),
      ("imageUrl", toGVal $ imageUrl release),
      ("labels", toGVal . L.intercalate ", " . labels $ release),
      ("uri", toGVal $ uri release)
    ]

instance FromJSON Release where
  parseJSON (Object v) = do
    artists <- v .: "artists" >>= traverse (.: "name")
    title <- v .: "title"
    year <- v .: "year"
    released <- v .: "released"
    images <- v .: "images"
    imageUrl <- case images of
        (img:_) -> img .: "resource_url"
        []      -> fail "No images found"
    labels <- v .: "labels" >>= traverse (.: "name")
    uri <- v .: "uri"
    return Release {
      artists = artists,
      title = title,
      year = year,
      released = released,
      imageUrl = imageUrl,
      labels = labels,
      uri = uri
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

data ReleaseType = Master | Regular deriving (Show)

toQueryParams :: ReleaseType -> (String, String)
toQueryParams Master  = ("master", "master_id")
toQueryParams Regular = ("release", "id")

fetchReleaseIdByType :: String -> String -> ReleaseType -> IO String
fetchReleaseIdByType artistName albumName releaseType = do
  let url = "https://api.discogs.com/database/search"
      (queryType, queryKey) = toQueryParams releaseType
      query =
        [ ("artist", Just $ BS.pack artistName),
          ("release_title", Just $ BS.pack albumName),
          ("type", Just $ BS.pack queryType)
        ]
  body <- BS.unpack <$> runDiscogsQuery query url
  case body ^? key "results" . nth 0 . key (fromString queryKey) . _Integer of
    Just idValue -> return $ show idValue
    Nothing      -> fail $ "Failed to extract " ++ queryKey ++ " from the response"

fetchMainReleaseId :: String -> IO String
fetchMainReleaseId masterId = do
  let url = "https://api.discogs.com/masters/" ++ masterId
  body <- BS.unpack <$> runDiscogsQuery [] url
  case body ^? key "main_release" . _Integer of
    Just mainId -> return $ show mainId
    Nothing     -> fail "Failed to extract main_release from the response"

-- | Fetches the master release ID. If fetching the master release ID fails,
-- it falls back to fetching the first item's ID where the type is 'release'.
-- This ensures that a valid release ID is returned even if the master release
-- ID is unavailable.
getReleaseId :: String -> String -> IO String
getReleaseId artistName albumName = do
  result <- try (fetchReleaseIdByType artistName albumName Master) :: IO (Either SomeException String)
  case result of
    Right masterId -> fetchMainReleaseId masterId
    Left _         -> fetchReleaseIdByType artistName albumName Regular

getRelease :: String -> IO Release
getRelease releaseId = do
  let url = "https://api.discogs.com/releases/" ++ releaseId
  body <- runDiscogsQuery [] url
  case (decodeStrict body :: Maybe Release) of
    Just release -> return release
    Nothing      -> fail "Cannot decode main release"

-- Template rendering
nullResolver :: IncludeResolver Identity
nullResolver = const $ return Nothing

getTemplate :: String -> Template SourcePos
getTemplate content = either (error . show) id . runIdentity $
  parseGinger nullResolver Nothing content

templatePath :: IO String
templatePath = do
  progName <- getProgName
  return $ takeDirectory progName ++ "/app/templates/post.md"

runGenAlbumPost :: String -> String -> IO String
runGenAlbumPost artistName albumName = do
  release <- getReleaseId artistName albumName >>= getRelease
  content <- templatePath >>= readFile
  return $ T.unpack . easyRender release $ getTemplate content

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    [artistName, albumName, branchName] -> do
      result <- try $ runGenAlbumPost artistName albumName :: IO (Either SomeException String)
      post <- case result of
        Left _ -> do
          _ <- putStrLn "Cannot get album info from Discog, falling back to default post template"
          templatePath >>= readFile
        Right output -> return output
      writeFile branchName post
      putStrLn "done"
    _ -> putStrLn "Usage: pull_album_info <artist_name> <album_name> <branch_name>"
