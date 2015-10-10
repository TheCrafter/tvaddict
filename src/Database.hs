{-# LANGUAGE DeriveGeneric #-}

module Database
( Episode (..)
, Database.Series (..)
, createDb
, insertSeries
, deleteSeriesByTitle
, updateSeriesByTitle
, findSeriesByTitle
, readDb
) where

import Data.Text  as T
import GHC.Generics
import GHC.List
import qualified Data.Aeson           as Ae
import qualified Data.ByteString.Lazy as BS

-- The filepath for our database JSON file
dbFilepath = "C:/Users/TheCrafter/AppData/roaming/.tvaddict"

-- | Information about a single episode
data Episode = Episode { season :: Int -- ^ Season number of that episode
                       , epNum  :: Int -- ^ Number of the episode
                       } deriving (Show, Generic)

-- | Information about series
data Series = Series { title      :: String  -- ^ The series title
                     , curEpisode :: Episode -- ^ Our last watched episode
                     } deriving (Show, Generic)

-- JSON Parsing instances. Compiler auto-generats everything
-- thanks to Generics
instance Ae.FromJSON Episode
instance Ae.ToJSON Episode where
  toJSON = Ae.genericToJSON Ae.defaultOptions
instance Ae.FromJSON Series
instance Ae.ToJSON Series where
  toJSON = Ae.genericToJSON Ae.defaultOptions

-- | Creates a new database file and feels it with the input list
createDb :: [Series] -> IO ()
createDb s =
  BS.writeFile dbFilepath $ Ae.encode s


-- | Adds one or more series to database file
insertSeries :: [Series] -> Maybe [Series] -> IO ()
insertSeries _ Nothing               = return ()
insertSeries newSeries (Just series) = createDb $ newSeries ++ series

-- | Deletes one or more series from database
deleteSeriesByTitle :: String -> Maybe [Series] -> IO (Maybe [Series])
deleteSeriesByTitle _ Nothing         = return Nothing
deleteSeriesByTitle str (Just series) = do
  let s = GHC.List.filter (\s' -> str /= title s') series
  if (GHC.List.length s) == (GHC.List.length series)
    then return Nothing
    else do
      createDb s
      return $ Just s

-- | Updates series already in database by using their title for
--   searching
updateSeriesByTitle :: String         -- ^ Title of the series to update
                    -> Series         -- ^ The actual update
                    -> Maybe [Series] -- ^ Previous series list
                    ->  IO ()
updateSeriesByTitle _ _ Nothing           = return ()
updateSeriesByTitle title s (Just series) =
  createDb $ updateSeriesList s series

-- | Search the database for series that match given title
findSeriesByTitle :: String -> Maybe [Series] -> IO (Maybe [Series])
findSeriesByTitle _ Nothing         = return Nothing
findSeriesByTitle str (Just series) = do
  let series' = GHC.List.filter (\s -> str == title s) series
  case series' of
    [] -> return Nothing
    x  -> return $ Just x

-- | Read the database file and return its contents
readDb :: IO (Maybe [Series])
readDb =  Ae.decode <$> BS.readFile dbFilepath

--------------------------------------------------
-- Non exported data
--------------------------------------------------

-- | Applies an update to a series list
updateSeriesList :: Series -> [Series] -> [Series]
updateSeriesList _ []     = []
updateSeriesList s (x:xs) = if (title s) == (title x)
                               then [s] ++ xs
                               else [x] ++ (updateSeriesList s xs)
