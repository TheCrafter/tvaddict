{-# LANGUAGE DeriveGeneric #-}

module Database
( Episode (..)
, Database.Series (..)
, createDbFile
, insertSeries
, deleteSeriesByTitle
, updateSeriesByTitle
, findSeriesByTitle
, readDbFile
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
createDbFile s =
  BS.writeFile dbFilepath $ Ae.encode s


insertSeries :: [Series] -> Maybe [Series] -> IO ()
insertSeries _ Nothing = return ()
insertSeries newSeries (Just series) =
  createDbFile $ newSeries ++ series

deleteSeriesByTitle :: String -> Maybe [Series] -> IO (Maybe [Series])
deleteSeriesByTitle _ Nothing         = return Nothing
deleteSeriesByTitle str (Just series) =
  let s = GHC.List.filter (\s' -> str /= title s') series
  in
    if (GHC.List.length s) == (GHC.List.length series)
      then return Nothing
      else do
        createDbFile s
        return $ Just s


updateSeriesByTitle :: String -> Series -> Maybe [Series] ->  IO ()
updateSeriesByTitle _ _ Nothing           = return ()
updateSeriesByTitle title s (Just series) =
  createDbFile $ updateSeriesList s series


findSeriesByTitle :: String -> Maybe [Series] -> IO (Maybe [Series])
findSeriesByTitle _ Nothing         = return Nothing
findSeriesByTitle str (Just series) =
  let series' = GHC.List.filter (\s -> str == title s) series
  in
    case series' of
      [] -> return Nothing
      x  -> return $ Just x

readDbFile :: IO (Maybe [Series])
readDbFile = do
  result <- Ae.eitherDecode <$> BS.readFile dbFilepath
  case result of
    Left _  -> return Nothing
    Right x -> return $ Just x

updateSeriesList :: Series -> [Series] -> [Series]
updateSeriesList _ []     = []
updateSeriesList s (x:xs) = if (title s) == (title x)
                               then [s] ++ xs
                               else [x] ++ (updateSeriesList s xs)
