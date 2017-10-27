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

_smartPlayer :: Int -> IORef (String, String) -> Maybe [Int] -> IO String
_smartPlayer n gameState p = do
    (lastGuess,letters) <- readIORef gameState
--  sacar las letras de letters que nos dieron res=0
    let newLetters = compareList lastGuess p
    newGuess <- randomStr n newLetters
    writeIORef gameState (newGuess,newLetters)
    print lastGuess
    print p
    print newLetters
--     (test return lastGuess p)
--     print lastGuess
--     print newGuess
--     print p
    return newGuess

compareList :: String -> Maybe [Int] -> String
compareList (s:string) (Just (r:results)) = if r /= 0 then (s:compareList string (Just results)) else (compareList string (Just results))
compareList _ _ = []


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
