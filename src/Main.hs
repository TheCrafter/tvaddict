{-# LANGUAGE OverloadedStrings #-}

import Database as Db
import Data.Aeson as Ae
import System.Exit

data MenuItem = MenuItem { msg   :: String
                         , cmd   :: IO ()
                         }

data Menu = Menu { items  :: [MenuItem]
                 , header :: String
                 }

instance Show MenuItem where
  show = msg

instance Show Menu where
  show (Menu i h) = h ++ "\n" ++ (showItems 0 i)

    where
      showItems index strList =
        case strList of
          []     -> ""
          (x:xs) -> (show index) ++ ". " ++ (show x) ++ "\n" ++ showItems (index + 1) xs

runMenuCmd :: Int -> IO ()
runMenuCmd i = cmd $ (items myMenu) !! i

myMenuItems =
  [ (MenuItem "View all Series" printSeries)
  , (MenuItem "Add series" addSeries)
  , (MenuItem "Delete series" (putStrLn "TODO"))
  , (MenuItem "Exit" exitSuccess)
  ]

myMenu = Menu myMenuItems "Choose one of the following:"

printSeries :: IO ()
printSeries = do
  series <- Db.readDbFile
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
  putStrLn "Write the name of the series you want to add"
  name <- getLine

  putStrLn "Write the number of the season you're at"
  season' <- getLine
  let season = read season' :: Int

  putStrLn "Write the number of the episode you're at"
  episode' <- getLine
  let episode = read episode' :: Int

  let toAdd = Series name $ Episode season episode
  series <- Db.readDbFile

  -- Create dbfile if it doesnt exist
  case series of
    Nothing -> Db.createDbFile [toAdd]
    Just _  -> Db.insertSeries [toAdd] series

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
