module GuessGame where

import System.Random

type Player = Maybe [Int] -> IO String

consolePlayer Nothing = getLine
consolePlayer (Just chk) = do
   putStrLn chk
   getLine
guessGame :: Int -> String -> Int -> Player -> IO Int
guessGame len chars tries player = do
   str <- randomStr len chars
   _guessGame str tries Nothing player


_guessGame :: String -> Int -> Maybe [Int] -> Player
   -> IO Int
_guessGame s t x p =
   if t <= 0 || x == Just (take (length s) (repeat 2))
   then return (max 0 t) else do
      m <- p x
      _guessGame s (t - 1) (Just (checkStr s m)) p


checkStr :: String -> String -> [Int]
checkStr txt ln = zipWith f txt ln
  where f c1 c2 = if c1 == c2 then 2 else if elem c2 txt then 1 else 0


randomStr :: Int -> String -> IO String
randomStr n chars = do
    g <- newStdGen
    return (take n [chars !! i | i <- randomRs (0, (length chars) -1) g])