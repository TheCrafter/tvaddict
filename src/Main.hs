

dbFilepath = "C:/Users/TheCrafter/AppData/roaming/.tvaddict"

data Episode = Episode { season :: Int
                       , epNum  :: Int
                       }

instance Show Episode where
  show e = "S" ++ (show $ season e) ++ ".E" ++ (show $ epNum e)

data Series = Series { title      :: String
                     , curEpisode :: Episode
                     }

instance Show Series where
  show s = "Title: " ++ (title s) ++ "\nLast Episode watched: " ++ (show $ curEpisode s)

mySeries :: [Series]
mySeries = [ (Series "Gotham" (Episode 2 3))
           , (Series "Person of Interest" (Episode 4 22))
           ]

exportSeries :: (String -> IO()) -> [Series] -> IO ()
exportSeries _ []     = return ()
exportSeries f (x:xs) = do
  f $ show x ++ "\n"
  exportSeries f xs

main = do
 exportSeries putStrLn mySeries
 exportSeries (writeFile dbFilepath) mySeries
