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
   = Fire
   | Water
   | Air
   deriving Show

data Mage
  = MkMage { classe :: Class
            ,item   :: Item
            }

getMage :: String -> Maybe Mage
getMage "mago" = Just MkMage{ classe = Mage, item = Staff}
getMage "knight" = Just MkMage{ classe = Knight, item = Sword} 
getMage "rogue" = Just MkMage{ classe = Rogue, item = Knives} 
getMage x    = Nothing

getClass :: String -> Maybe Class
getClass "mago"       = Just Mage
getClass "cavaleiro"    = Just Knight
getClass "assassino"    = Just Rogue
getClass "arqueiro"     = Just Archer
getClass x              = Nothing

getTeste :: Class -> Maybe Item
getTeste Mage      = Just Staff
getTeste x              = Nothing

getGear :: String -> Maybe Item
getGear "espada"  = Just Sword
getGear "cajado"  = Just Staff
getGear "arco"    = Just Bow
getGear "lamina"  = Just Knives
getGear _         = Nothing

getAtk :: String -> Maybe AttackKind
getAtk "1"  = Just Fire
getAtk "2"  = Just Water
getAtk "3"  = Just Air
getAtk x    = Nothing

--                  \/ Esse IO carrega uma String dentro
prompt :: String -> IO String
prompt texto = do 
   putStrLn texto
   getLine -- "Retorna" o resultado do getLine


main :: IO ()
main = do
  mage <- prompt "Escolha sua classe: "
  case getMage mage of 
    Nothing -> do 
      putStrLn "Entrada inválida!"
      main
    Just gear -> do
      print $ "Sua classe eh " ++ show (classe gear) ++ " com a arma " ++ show (item gear)


      
