{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List                  (sortBy)
import           Data.List.Split            (splitOn)
import           Data.Maybe                 (catMaybes)
import           Data.Ord                   (Down (Down), comparing)
import qualified Data.Text                  as T
import           Data.Time                  (fromGregorian)
import           Data.Time.Calendar         (Day, addGregorianMonthsClip,
                                             toGregorian)
import           Data.Time.Clock            (getCurrentTime, utctDay)
import           System.Directory           (listDirectory)
import           Text.Pandoc                (Inline (Space, Str),
                                             Pandoc (Pandoc), readMarkdown,
                                             runIO)
import           Text.Pandoc.Options
import           Text.Pandoc.Writers.Shared (lookupMetaInlines)

parseDateFromString :: String -> Day
parseDateFromString filePath =
    let [year, month, day] = map read . take 3 . splitOn "-" $ filePath
    in fromGregorian (toInteger year) month day

data AlbumPostSummary = AlbumPostSummary {
    score    :: Float,
    released :: Day,
    title    :: String
} deriving (Eq, Ord)

instance Show AlbumPostSummary where
    show summary =
        title summary ++ " (score: " ++ show (score summary) ++ ", release date: " ++ show (released summary) ++ ")"

getAlbumPostSummary :: String -> IO (Maybe AlbumPostSummary)
getAlbumPostSummary filePath = do
    md <- T.pack <$> readFile ("posts/" ++ filePath)
    pandoc <- runIO $
        readMarkdown
        (def {
            readerStandalone = True,
            readerExtensions =
            enableExtension Ext_yaml_metadata_block (readerExtensions def) } )
        md
    case pandoc of
        Left _ -> return Nothing
        Right (Pandoc meta _) -> do
            if not . hasMusicTag $ lookupMetaInlines "tags" meta
            then do
                return Nothing
            else return $ Just AlbumPostSummary {
                released = parseDateFromString (getValue "released"),
                score = read (getValue "score") :: Float,
                title = getValue "title"
            }
            where
                hasMusicTag inlines = any (`elem` inlines) [Str "music", Str "music,"]
                getValue key = T.unpack . T.concat . map stringify . lookupMetaInlines key $ meta
                stringify (Str value) = value
                stringify Space       = " "
                stringify _           = ""

data What = Good | Wack | Ok deriving (Eq)
instance Show What where
    show Good = "====== good ======"
    show Wack = "====== wack ======"
    show Ok   = "======= ok ======="

data Bucket = Bucket {
    summaries :: [AlbumPostSummary],
    what      :: What
}


summariesToBuckets :: [AlbumPostSummary] -> ([AlbumPostSummary], [AlbumPostSummary], [AlbumPostSummary])
summariesToBuckets = foldr f ([], [], [])
    where
        f s (hi, mid, lo)  | score s >= 8.0 = (s:hi, mid, lo)
        f s (hi, mid, lo)  | score s < 6.0 = (hi, mid, s:lo)
        f s (hi, mid, lo)   = (hi, s:mid, lo)

printBucket :: Bucket -> IO ()
printBucket bucket =
    if null (summaries bucket)
    then return ()
    else do
        putStrLn ""
        print (what bucket)
        mapM_ print $ summaries bucket
        putStrLn ""

main :: IO ()
main = do
    files <- listDirectory "posts/"
    (year, month, _) <- toGregorian . utctDay <$> getCurrentTime
    let endDate = fromGregorian year month 1
        startDate = addGregorianMonthsClip (-1) endDate
        filteredFiles = filter ((\fileDate -> fileDate >= startDate && fileDate < endDate) . parseDateFromString) files
    allSummaries <- sortBy (comparing Down) . catMaybes <$> mapM getAlbumPostSummary filteredFiles
    let (hiScores, midScores, lowScores) = summariesToBuckets allSummaries
    mapM_ printBucket
        [ Bucket hiScores Good
        , Bucket midScores Ok
        , Bucket lowScores Wack
        ]
