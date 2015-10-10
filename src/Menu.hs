module Menu
( MenuItem (..)
, Menu (..)
, runMenuCmd
) where

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

runMenuCmd :: Menu -> Int -> IO ()
runMenuCmd m i = cmd $ (items m) !! i
