 module Main where

import Lib

-- Armas do personagem
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

data Player = Player { classePlayer       :: Class
                       , namePlayer       :: String 
                       , itemPlayer       :: Item 
                       , attackPlayer     :: Int
                       , lifePlayer       :: Int
                       , maxlifePlayer    :: Int
                       , levelPlayer      :: Int
                       , expPlayer        :: Int
                       , controlExpPlayer :: Int
                       , protectionPlayer :: Int
                       , livePlayer       :: Bool
                       } deriving Show

data Enemy = Enemy { lifeEnemy        :: Int 
                       , nameEnemy        :: String 
                       , atkEnemy         :: Int 
                       , xpDropEnemy      :: Int 
                       , levelEnemy       :: Int 
                       , protectionEnemy  :: String
                       , liveEnemy        :: Bool 
                       } deriving Show

getPlayer :: String -> Player
getPlayer name = Player Mage name Sword 10 300 300 1 0 100 10 True

getEnemy :: String -> Enemy
getEnemy name = Enemy 300 name 15 20 1 0 True

-- Adiciona xp no player
countExp :: Int -> Player -> Player
countExp exp i = Player (classePlayer i) (namePlayer i) (itemPlayer i) (attackPlayer i) (lifePlayer i) (maxlifePlayer i ) (levelPlayer i) (expPlayer i) (controlExpPlayer i) (protectionPlayer i) (livePlayer i)

--Quando o player sobe de nivel
updatePlayer :: Player -> Player
updatePlayer user
  | (expPlayer user) >= (controlExpPlayer user) = setHpMax (updateControlExpPlayer (updateLevelPlayer (updateProtectionPlayer (updateAttackPlayer (updateMaxHpPlayer user)))))
  | otherwise = user

-- Fazer alteracoes com o updatePlayer
updateMaxHpPlayer :: Player -> Player
updateMaxHpPlayer i = Player (classePlayer i) (namePlayer i) (itemPlayer i) (attackPlayer i) (lifePlayer i) (((maxlifePlayer i) `div`5) + maxlifePlayer i) (levelPlayer i) (expPlayer i) (controlExpPlayer i) (protectionPlayer i) (livePlayer i)

updateAttackPlayer :: Player -> Player
updateAttackPlayer i = Player (classePlayer i) (namePlayer i) (itemPlayer i) ((attackPlayer i) + ((attackPlayer i)`div`5)) (lifePlayer i) (maxlifePlayer i ) (levelPlayer i) (expPlayer i) (controlExpPlayer i) (protectionPlayer i) (livePlayer i)

updateProtectionPlayer :: Player -> Player
updateProtectionPlayer i = Player (classePlayer i) (namePlayer i) (itemPlayer i) (attackPlayer i) (lifePlayer i) (maxlifePlayer i ) (levelPlayer i) (expPlayer i) (controlExpPlayer i) ((protectionPlayer i) + ((protectionPlayer i)`div`5)) (livePlayer i)

updateLevelPlayer :: Player -> Player
updateLevelPlayer i = Player (classePlayer i) (namePlayer i) (itemPlayer i) (attackPlayer i) (lifePlayer i) (maxlifePlayer i ) ((levelPlayer i) + 1) (expPlayer i) (controlExpPlayer i) (protectionPlayer i) (livePlayer i)

updateControlExpPlayer :: Player -> Player
updateControlExpPlayer i = Player (classePlayer i) (namePlayer i) (itemPlayer i) (attackPlayer i) (lifePlayer i) (maxlifePlayer i ) (levelPlayer i) (expPlayer i) ((controlExpPlayer i) * 2) (protectionPlayer i) (livePlayer i)

--Deixa o player full life
setHpMax :: Player -> Player
setHpMax i = Player (classePlayer i) (namePlayer i) (itemPlayer i) (attackPlayer i) (lifePlayer i) (maxlifePlayer i ) (levelPlayer i) (expPlayer i) (controlExpPlayer i) (protectionPlayer i) (livePlayer i)

--Calcular dano
calculateDamage :: a -> a -> Int
calculateDamage damage protection = round ((damage / ((protection / 100) + 1)))

--Atualizar vida quando toma dano
receiveDamageEnemy :: Int -> Player -> Player
receiveDamageEnemy damage i
  | (((lifePlayer i) - damage) <= 0) = Player (classePlayer i) (namePlayer i) (itemPlayer i) (attackPlayer i) ((lifePlayer i) - damage) (maxlifePlayer i ) (levelPlayer i) (expPlayer i) (controlExpPlayer i) (protectionPlayer i) (False)
  | otherwise = Player (classePlayer i) (namePlayer i) (itemPlayer i) (attackPlayer i) ((lifePlayer i) - damage) (maxlifePlayer i ) (levelPlayer i) (expPlayer i) (controlExpPlayer i) (protectionPlayer i) (livePlayer i)

receiveDamagePlayer :: Int -> Player -> Player
receiveDamagePlayer damage j
  | (lifeEnemy j - damage) <= 0 = Enemy (nameEnemy j) (lifeEnemy j - damage) (atkEnemy j) (xpDropEnemy j) (levelEnemy j) (protectionEnemy j) (False)
  | otherwise = Enemy (nameEnemy j) (lifeEnemy j - damage) (atkEnemy j) (xpDropEnemy j) (levelEnemy j) (protectionEnemy j) (liveEnemy j)

printSeparator = putStrLn "-------------------------------------------------------------"

printAction = do
                putStrLn ""
                printSeparator
                putStrLn "Selecione uma ação :"
                putStrLn "1 - Atague fisico "
                putStrLn "2 - Cura  "
                printSeparator
                putStrLn "Digite o numero referente a ação desejada : "
                putStrLn ""

printEscolhas :: Int -> IO()
printEscolhas 1 = putStrLn "Você ataca o inimigo fisicamente"
printEscolhas 2 = putStrLn "Você sente que precisa recuperar suas energias"
printEscolhas x = putStrLn "Opção invalida : por favor selecione uma opção valida "

{-Nesse caso, nao estamos tratando ainda as multiplas escolhas, estamos fazendo "de conta" que a escolha é sempre a numero 1 (atacar inimigo)-}
managerChoose 1 user enemy = do
  printEscolhas 1
  return (user, receiveDamagePlayer (attackPlayer user) enemy)


--                  \/ Esse IO carrega uma String dentro
prompt :: String -> IO String
prompt texto = do 
   putStrLn texto
   getLine -- "Retorna" o resultado do getLine

main :: IO ()
main = do
  person <- prompt "Select you class: (Mage, Knight, Rogue or Archer)"
  case getPlayer person of 
    Nothing -> do 
      putStrLn "Please, choose a correct class"
      main
    Just player -> do 
      putStrLn $ "Your class: " ++ show (classe player) ++ 
                 "\nYour gear: " ++ show (item player) 
      getEquip

getEnemy :: IO ()
getEnemy = do 
  gear <- prompt "Apareceu um Enemy, lute com ele: " 
  case getPlayer of
     player -> do
      putStrLn $ "voce perdeu x vida, vida atual " ++ show (life player)
      getAttack

getAttack :: IO ()
getAttack = do 
  atk <- prompt "Escolha seu ataque: " 
  case getPlayer atk of
    Nothing -> do 
      putStrLn "Entrada inválida!"
      getAttack
    Just elemento -> do
      putStrLn "Ainda não fez essa parte lel"

getEquip :: IO ()
getEquip = do 
  gear <- prompt "Escolha sua arma: " 
  case getPlayer gear of
    Nothing -> do 
      putStrLn "Entrada inválida!"
      getEquip
    Just arma -> do
      getAttack

teste :: IO ()
teste = do 
  gear <- prompt "teste: " 
  case getPlayer gear of
    Nothing -> do 
      putStrLn "Entrada inválida!"
      teste
    Just arma -> do
      getAttack
      
