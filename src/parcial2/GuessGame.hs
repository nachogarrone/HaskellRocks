module GuessGame where

import Data.List
import System.Random
import Data.IORef

type Player = Maybe [Int] -> IO String



randomPlayer :: Int -> String -> Player
randomPlayer n s _ = randomStr n s

smartPlayer :: Int -> String -> IO Player
smartPlayer n s = do
    gameState <- newIORef ("",s)
    return (_smartPlayer n gameState)

_smartPlayer n gameState p = do
    (_,letters) <- readIORef gameState
    newGuess <- randomStr n letters
    writeIORef gameState (newGuess,letters)
    (readIORef gameState) >>= print
    return newGuess

compareList _ [] = return []
compareList (s:string) (r:results) = do
    if r == 0 then do s else print "no sacar nada"
    compareList string results


consolePlayer Nothing = getLine
consolePlayer (Just chk) = do
   putStrLn chk
   getLine

guessGame :: Int -> String -> Int -> Player -> IO Int
guessGame len chars tries player = do
   str <- randomStr len chars
   _guessGame str tries Nothing player

_guessGame :: String -> Int -> Maybe [Int] -> Player -> IO Int
_guessGame s t x p = if t <= 0 || x == Just (take (length s) (repeat 2))
    then return (max 0 t)
    else do
      m <- p x
      _guessGame s (t - 1) (Just (checkStr s m)) p





checkStr :: String -> String -> [Int]
checkStr txt ln = zipWith f txt ln
  where f c1 c2 = if c1 == c2 then 2 else if elem c2 txt then 1 else 0


randomStr :: Int -> String -> IO String
randomStr n chars = do
  g <- newStdGen
  return $ take n [chars !! i | i <- randomRs (0, length chars - 1) g]
