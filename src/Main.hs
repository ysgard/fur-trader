{-|
Module : FurTrader
Description: Fur Trader - Port from  Basic Computer Games
Copyright: (c) Jan Van Uytven, 2015
License: MIT
Maintainer : ysgard@gmail.com
Stability: experimental
Portability: POSIX

-}
module Main where

import Data.Char (toUpper)
import System.Exit (exitSuccess)
import System.IO (stdout, hFlush)



data Fur = Mink | Beaver | Ermine | Fox
         deriving (Show)

data Baggage = Baggage { mink :: Int
                       , beaver :: Int
                       , ermine :: Int
                       , fox :: Int
                       , dollars :: Double
                       } deriving (Show)

-- |Intro to the game
intro :: IO ()
intro = do
  putStrLn $ take 16 (repeat ' ') ++ "FUR TRADER"
  putStrLn $ "CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY"
  putStrLn $ take 6 (repeat ' ') ++ "Haskell port by Jan Van Uytven"
  putStrLn ""
  putStrLn "YOU ARE THE LEADER OF A FRENCH FUR TRADING EXPEDITION IN"
  putStrLn "1776 LEAVING THE LAKE ONTARIO AREA TO SELL FURS AND GET"
  putStrLn "SUPPLIES FOR THE NEXT YEAR.  YOU HAVE A CHOICE OF THREE"
  putStrLn "FORTS AT WHICH YOU MAY TRADE.  THE COST OF SUPPLIES"
  putStrLn "AND THE AMOUNT YOU RECEIVE FOR YOUR FURS WILL DEPEND"
  putStrLn "ON THE FORT THAT YOU CHOOSE."

initialPelts :: IO Baggage
initialPelts = do
  


ask :: String -> IO String
ask prompt = do
  putStr $ prompt ++ " ? "
  hFlush stdout
  getLine

yesNo :: IO Bool
yesNo = do
  answer <- ask "ANSWER YES OR NO"
  case (head $ map toUpper answer) of
   'Y' -> return True
   'N' -> return False
   _   -> yesNo

oneTwoThree :: IO Int
oneTwoThree = do
  answer <- ask "ANSWER 1, 2 or 3."
  case (head answer) of
   '1' -> return 1
   '2' -> return 2
   '3' -> return 3
   _ -> oneTwoThree

turn :: IO ()


main :: IO ()
main = do
  intro
  putStrLn "DO YOU WISH TO TRADE FURS?"
  startGame <- yesNo
  if startGame
    then turn
    else exitSuccess
  
