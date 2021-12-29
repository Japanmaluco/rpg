module Main where

import Lib

data Item 
  = Sword
  | Staff 
  | Bow
  | Knives
  deriving Show

data Class
    = Mage
    | Knight
    | Rogue
    | Archer
    deriving Show

data AttackKind 
   = Slice
   | Fire
   | Enshroud
   | Heavy_shot
   deriving Show

data User
  = MkUser { classes:: Class
            ,item   :: Item
            ,attack :: AttackKind
            }

getUser :: String -> Maybe User
getUser "mage" = Just MkUser{ classes   = Mage
                            , item      = Staff
                            , attack    = Fire
                            }
getUser "knight" = Just MkUser{ classes = Knight
                              , item    = Sword
                              , attack  = Slice
                              }
getUser "rogue" = Just MkUser{ classes  = Rogue
                              , item    = Knives
                              , attack  = Enshroud
                              }
getUser "archer" = Just MkUser{ classes = Archer
                              , item    = Bow
                              , attack  = Heavy_shot} 
getUser x    = Nothing



--                  \/ Esse IO carrega uma String dentro
prompt :: String -> IO String
prompt texto = do 
   putStrLn texto
   getLine -- "Retorna" o resultado do getLine


main :: IO ()
main = do
  job <- prompt "Choose you class: (Archer, Rogue, Mage and Knight)"
  case getUser job of 
    Nothing -> do 
      putStrLn "Input invalid!"
      main
    Just gear -> do
      putStrLn $ "Your class is " ++ show (classes gear) ++ " with the gear " ++ show (item gear)

{- nickname :: IO ()
nickname = do
  mage <- prompt "Escolha seu nome: "
  case getUser mage of 
    Nothing -> do 
      putStrLn "Entrada inválida!"
      nickname
    Just mage -> do
      print $ "Your nick is " ++ show (nick mage) -}


      
