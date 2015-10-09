{-# LANGUAGE OverloadedStrings #-}

import Database as Db
import Data.Aeson as Ae
import System.Exit

mySeries :: [Db.Series]
mySeries = [ (Series "Gotham" (Episode 2 3))
           , (Series "Person of Interest" (Episode 4 22))
           , (Series "Supernatural" (Episode 1 11))
           ]

searchTitle = "Flash"

main = do
   putStrLn "Creating new .tvaddict file..."
   Db.createDbFile mySeries

   putStrLn "Adding Flash..."
   Db.insertSeries [Series "Flash" (Episode 1 2)]

   find

   putStrLn "Updating Flash..."
   Db.updateSeriesByTitle searchTitle $ Series "Flash" $ Episode 0 0

   find

  where
    find = do
      putStrLn $ "Trying to find " ++ searchTitle ++ "..."
      result <- Db.findSeriesByTitle searchTitle
      case result of
        Nothing -> putStrLn $ "Couldn't find " ++ searchTitle
        Just x  -> putStrLn $ show x
