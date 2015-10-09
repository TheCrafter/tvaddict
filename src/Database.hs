{-# LANGUAGE DeriveGeneric #-}

module Database
( Episode (..)
, Database.Series (..)
, createDbFile
, readDbFile
) where

import qualified Data.Aeson as Ae
import Data.Text  as T
import qualified Data.ByteString.Lazy as BS
import GHC.Generics

dbFilepath = "C:/Users/TheCrafter/AppData/roaming/.tvaddict"

data Episode = Episode { season :: Int
                       , epNum  :: Int
                       } deriving (Show, Generic)

data Series = Series { title      :: !T.Text
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

readDbFile :: IO (Either String [Series])
readDbFile = Ae.eitherDecode <$> BS.readFile dbFilepath

