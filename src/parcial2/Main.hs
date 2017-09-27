module Main where

import Algebraicos

main :: IO ()
main = do
    print (monthDays Feb 2017)
    print (monthDays Feb 2016)
    print (monthDays Feb 2015)
