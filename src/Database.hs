{-# LANGUAGE DeriveGeneric #-}

module Database
( Episode (..)
, Database.Series (..)
, createDbFile
, insertSeries
, deleteSeriesByTitle
, findSeriesByTitle
) where

import qualified Data.Aeson as Ae
import Data.Text  as T
import qualified Data.ByteString.Lazy as BS
import GHC.Generics
import GHC.List

dbFilepath = "C:/Users/TheCrafter/AppData/roaming/.tvaddict"

data Episode = Episode { season :: Int
                       , epNum  :: Int
                       } deriving (Show, Generic)

data Series = Series { title      :: String
                     , curEpisode :: Episode
                     } deriving (Show, Generic)

instance Ae.FromJSON Episode
instance Ae.ToJSON Episode where
  toJSON = Ae.genericToJSON Ae.defaultOptions
instance Ae.FromJSON Series
instance Ae.ToJSON Series where
  toJSON = Ae.genericToJSON Ae.defaultOptions

createDbFile :: [Series] -> IO ()
createDbFile s = do
  BS.writeFile dbFilepath $ Ae.encode s

insertSeries :: [Series] -> IO ()
insertSeries s = do
  curSeries <- readDbFile
  case curSeries of
    Left err -> return ()
    Right s' -> createDbFile $ s ++ s'

deleteSeriesByTitle :: String -> IO ()
deleteSeriesByTitle str = do
  curSeries <- readDbFile
  case curSeries of
    Left err -> return ()
    Right s  -> do
      let s' = GHC.List.filter (\s'' -> str /= title s'') s
      createDbFile s'

findSeriesByTitle :: String -> IO (Maybe Series)
findSeriesByTitle str = do
  result <- readDbFile
  case result of
    Left err -> return Nothing
    Right series -> do
      let seriesList = GHC.List.filter (\s -> str == title s) series
      let listLength = GHC.List.length seriesList
      case listLength of
        1 -> return $ Just $ GHC.List.head seriesList
        _ -> return Nothing

readDbFile :: IO (Either String [Series])
readDbFile = Ae.eitherDecode <$> BS.readFile dbFilepath
