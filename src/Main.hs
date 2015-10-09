{-# LANGUAGE OverloadedStrings #-}

import Database as Db
import Data.Aeson as Ae
import System.Exit

mySeries :: [Db.Series]
mySeries = [ (Series "Gotham" (Episode 2 3))
           , (Series "Person of Interest" (Episode 4 22))
           , (Series "Supernatural" (Episode 1 11))
           ]

searchTitle = "Gotham"

main = do
   putStrLn "Creating new .tvaddict file..."
   Db.createDbFile mySeries

   putStrLn $ "Trying to find " ++ searchTitle ++ "..."
   result <- Db.findSeriesByTitle searchTitle
   case result of
     Nothing -> putStrLn $ "Couldn't find " ++ searchTitle
     Just x  -> putStrLn $ show x
