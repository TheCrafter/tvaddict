{-# LANGUAGE OverloadedStrings #-}

import Database as Db
import qualified Menu as M
import Data.Aeson as Ae
import System.Exit

runMenuCmd :: Int -> IO ()
runMenuCmd = M.runMenuCmd myMenu

myMenuItems =
  [ (M.MenuItem "View all Series" printSeries)
  , (M.MenuItem "Add series"      addSeries)
  , (M.MenuItem "Delete series"   deleteSeries)
  , (M.MenuItem "Exit"            exitSuccess)
  ]

myMenu = M.Menu myMenuItems "Choose one of the following:"

printSeries :: IO ()
printSeries = do
  series <- Db.readDb
  case series of
    Nothing -> putStrLn "Couldn't read database file"
    Just s  -> putStrLn $ showSeries s

  where
    showSeries s =
      case s of
        []     -> ""
        (x:xs) -> showSerie x ++ "\n" ++ showSeries xs

    showSerie x =
      (title x) ++ ": Season " ++ (show $ season $curEpisode x) ++ ", Episode " ++ (show $ epNum $ curEpisode x)

addSeries :: IO ()
addSeries = do
  toAdd  <- getSeriesInfo
  series <- Db.readDb

  -- Create dbfile if it doesnt exist
  case series of
    Nothing -> Db.createDb [toAdd]
    Just _  -> Db.insertSeries [toAdd] series

deleteSeries :: IO ()
deleteSeries = do
  putStrLn "Write the title of the series you want to delete"
  title <- getLine

  Db.readDb >>= Db.deleteSeriesByTitle title >>= failedOperationCheck
  return ()

getSeriesInfo :: IO (Db.Series)
getSeriesInfo = do
  putStrLn "Write the name of the series you want to add"
  name <- getLine

  putStrLn "Write the number of the season you're at"
  season' <- getLine
  let season = read season' :: Int

  putStrLn "Write the number of the episode you're at"
  episode' <- getLine
  let episode = read episode' :: Int

  return $ Series name $ Episode season episode

failedOperationCheck :: Maybe x -> IO (Maybe x)
failedOperationCheck Nothing  = do
  putStrLn "Operation failed."
  return Nothing
failedOperationCheck x        = return x

main = do
  putStrLn $ show myMenu

  answer <- getLine
  case answer of
    "0" -> runMenuCmd 0
    "1" -> runMenuCmd 1
    "2" -> runMenuCmd 2
    "3" -> runMenuCmd 3
    _   -> putStrLn "Invalid choice"

  main
