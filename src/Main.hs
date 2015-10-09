{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson as Ae
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

instance FromJSON Episode
instance ToJSON Episode where
  toJSON = genericToJSON defaultOptions
instance FromJSON Main.Series
instance ToJSON Main.Series where
  toJSON = genericToJSON defaultOptions

mySeries :: [Main.Series]
mySeries = [ (Main.Series "Gotham" (Episode 2 3))
           , (Main.Series "Person of Interest" (Episode 4 22))
           , (Main.Series "Supernatural" (Episode 1 11))
           ]

printSeries :: [Main.Series] -> IO()
printSeries [] = return ()
printSeries (x:xs) = do
  putStrLn $ show x
  printSeries xs

main = do
   putStrLn "Deleting previous .tvaddict file..."
   writeFile dbFilepath ""

   putStrLn "Creating new .tvaddict file..."
   BS.appendFile dbFilepath $ encode mySeries

   putStrLn "Reading .tvaddict file..."
   decoded <- (eitherDecode <$> BS.readFile dbFilepath) :: IO (Either String [Main.Series])
   case decoded of
     Left err -> putStrLn err
     Right s  -> printSeries s
