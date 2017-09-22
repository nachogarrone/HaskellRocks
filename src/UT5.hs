module UT5 where
-- Gabriel Caamaño , Fernando Torterolo, Pedro Fernandez

-- ~/UCU/simple$ runhaskell ./src/UT5.hs
import System.Random

main = readN

readN :: Int -> IO [String]
readN 0 = return []
readN n = do
  line <- getLine           --  string <- IO String
  liness <- readN (n-1)    --   [String]  <- IO [String]
  return (line : liness)   --  IO  (Srting : [String])

readMany :: IO [String]
readMany = do
  linea <- getLine
  if linea == "" then return [] else do
    liness <- readMany
    return (linea:liness)

randomStr :: Int -> String -> IO String
randomStr n chars = do
  g <- newStdGen
  return $ take n [chars !! i | i <- randomRs (0, length chars - 1) g]
--  let c = chars !! fst i
--  cs <- randomStr (n-1) chars
--  return (c:chars)

rollDice :: IO Int
rollDice = getStdRandom (randomR (1,6))  -- devuelve una lista infinita de numeros en este rango

checkStr :: String -> IO [Int]
checkStr str = do
  line <- getLine
  let indices = [0.. length line -1] in
    return [getNum i (line !! i) str | i<-indices]

-- 2 esta y en la posicion
-- 1 esta, pero no en la posicon
-- 0 no esta
getNum :: Int -> Char -> String -> Int
getNum i c s
 | c == (s!!i) = 2
 | c `elem` s = 1
 | otherwise = 0

guessGame :: Int -> String -> Int -> IO Bool
guessGame largo caracteres intentos = do
  line <- randomStr largo caracteres
  guessGameAux intentos line

guessGameAux :: Int -> String -> IO Bool
guessGameAux 0 _  = return False
guessGameAux n objetivo  = do
  resultado <- checkStr objetivo
  if elem 0 resultado || elem 1 resultado
     then guessGameAux (n-1) objetivo
     else return True

type PlayerFuntion = Maybe [Int] -> IO String
-- segunda version del juego
-- antes tomabo un string , pedia uno por consola,  [1,2,0,2,1,3]
-- 2 esta y en la posicion
-- 1 esta, pero no en la posicon
-- 0 no esta
-- checkStrNew "objetivo" "entrada"
-- < [1,0,1,0,0,0,0]
checkStrNew :: String -> String -> [Int]
checkStrNew objetivo entrada = [getNum i (entrada !! i) objetivo | i <- [0 .. (length entrada -1)]]

-- Devuelve 0 si te quedaste sin intentos
-- Cantidad de intentos que sobraron si le enbocaste
-- caracteres posibles para hacer la cadna
guessGameNew :: Int -> String -> Int -> PlayerFuntion -> IO Int
guessGameNew largo caracteres intentos player =
  do objetivo <- randomStr largo caracteres   -- cadena aleatoria a adivinar
     str <- player Nothing
     guessGameAuxNew intentos objetivo str player

guessGameAuxNew :: Int -> String -> String -> PlayerFuntion -> IO Int
guessGameAuxNew 0 _ _ _ = return 0
guessGameAuxNew intentos objetivo jugadaAnterior player =
  let resultado = checkStrNew objetivo jugadaAnterior in
      if elem 0 resultado || elem 1 resultado
        then do
          nuevaJugada <- player (Just resultado)
          guessGameAuxNew (intentos-1) objetivo nuevaJugada player
      else return (intentos-1)
