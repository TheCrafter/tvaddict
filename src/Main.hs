{-# LANGUAGE OverloadedStrings #-}

import Database as Db
import Data.Aeson as Ae

mySeries :: [Db.Series]
mySeries = [ (Series "Gotham" (Episode 2 3))
           , (Series "Person of Interest" (Episode 4 22))
           , (Series "Supernatural" (Episode 1 11))
           ]

printSeries :: [Db.Series] -> IO()
printSeries [] = return ()
printSeries (x:xs) = do
  putStrLn $ show x
  printSeries xs

main = do
   putStrLn "Creating new .tvaddict file..."
   Db.createDbFile mySeries

   putStrLn "Reading .tvaddict file..."
   decoded <- Db.readDbFile
   case decoded of
     Left err -> putStrLn err
     Right s  -> printSeries s
