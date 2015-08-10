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
import System.Random (getStdRandom, random, randomR)
import Text.Read (readMaybe)



data Fur = Mink | Beaver | Ermine | Fox
         deriving (Show)

data Fort = Hochelaga | Stadacona | NewYork

data Baggage = Baggage { mink :: Int
                       , beaver :: Int
                       , ermine :: Int
                       , fox :: Int
                       , dollars :: Int
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

fortChoice :: IO ()
fortChoice = do
  putStrLn "DO YOU WANT TO TRADE YOUR FURS AT FORT 1, FORT 2,"
  putStrLn "OR FORT 3?  FORT 1 IS FORT HOCHELAGA (MONTREAL)"
  putStrLn "AND IS UNDER THE PROTECTION OF THE FRENCH ARMY."
  putStrLn "FORT 2 IS FORT STADACONA (QUEBEC) AND IS UNDER THE"
  putStrLn "PROTECTION OF THE FRENCH ARMY.  HOWEVER, YOU MUST"
  putStrLn "MAKE A PORTAGE AND CROSS THE LACHINE RAPIDS."
  putStrLn "FORT 3 IS FORT NEW YORK AND IS UNDER DUTCH CONTROL."
  putStrLn "YOU MUST CROSS THORUGH IROQUOIS LAND."
  

getFurs :: Int -> IO Baggage
getFurs dollars = do
  putStrLn $ "YOU HAVE $ " ++ show dollars ++ " SAVINGS"
  putStrLn "AND 190 FURS TO BEGIN THE EXPEDITION."
  putStrLn ""
  putStrLn "YOUR 190 FURS ARE DISTRIBUTED AMONG THE FOLLOWING"
  putStrLn "KINDS OF PELTS: MINK, BEAVER, ERMINE AND FOX."
  putStrLn ""
  mink <- askFur Mink
  beaver <- askFur Beaver
  ermine <- askFur Ermine
  fox <- askFur Fox
  case (checkFurs mink beaver ermine fox) of
   True -> return $ Baggage mink beaver ermine fox dollars
   False -> do
     putStrLn "YOU MAY NOT HAVE THAT MANY FURS."
     putStrLn "DO NOT TRY TO CHEAT.  I CAN ADD."
     putStrLn "YOU MUST START AGAIN."
     getFurs dollars
  where
    checkFurs :: Int -> Int -> Int -> Int -> Bool
    checkFurs m b e f = (m + b + e + f) <= 190

askFur :: Fur -> IO Int
askFur f = do
  nf <- ask $ "HOW MANY " ++ map toUpper (show f) ++ " FURS DO YOU HAVE"
  case ((readMaybe nf) :: Maybe Int) of
   Just n -> return n
   Nothing -> do
     putStrLn "INVALID INPUT.  ENTER A NUMBER"
     askFur f

hochelagaDescription :: IO ()
hochelagaDescription = do
  putStrLn "YOU HAVE CHOSEN THE EASIEST ROUTE.  HOWEVER, THE FORT"
  putStrLn "IS FAR FROM ANY SEAPORT.  THE VALUE"
  putStrLn "YOU RECEIVE FOR YOUR FURS WILL BE LOW AND THE COST"
  putStrLn "OF SUPPLIES HIGHER THAN AT FORTS STADACONA OR NEW YORK."

stadaconaDescription :: IO ()
stadaconaDescription = do
  putStrLn "YOU HAVE CHOSEN A HARD ROUTE.  IT IS, IN COMPARISON,"
  putStrLn "HARDER THAN THE ROUTE TO HOCHELAGA BUT EASIER THAN"
  putStrLn "THE ROUTE TO NEW YORK.  YOU WILL RECEIVE AN AVERAGE VALUE"
  putStrLn "FOR YOUR FURS AND THE COST OF YOUR SUPPLIES WILL BE AVERAGE"

yorkDescription :: IO ()
yorkDescription = do
  putStrLn "YOU HAVE CHOSEN THE MOST DIFFICULT ROUTE.  AT"
  putStrLn "FORT NEW YORK YOU WILL RECEIVE THE HIGHEST VALUE"
  putStrLn "FOR YOUR FURS.  THE COST OF YOUR SUPPLIES"
  putStrLn "WILL BE LOWER THAN AT ALL THE OTHER FORTS."


saleResults :: Int -> Int -> Int -> Int -> IO ()
saleResults m b e f = do
  putStrLn $ "YOUR BEAVER SOLD FOR $" ++ show b
  putStrLn $ "YOUR FOX SOLD FOR $" ++ show f
  putStrLn $ "YOUR ERMINE SOLD FOR $" ++ show e
  putStrLn $ "YOUR MINK SOLD FOR $" ++ show m

reportExpenses :: String -> Int -> Int -> IO ()
reportExpenses loc s e = do
  putStrLn $ "SUPPLIES AT FORT " ++ loc ++ " COST $" ++ show s
  putStrLn $ "YOUR TRAVEL EXPENSES TO " ++ loc ++ " WERE $" ++ show e

processTrip :: Baggage -> Fort -> Maybe Int
processTrip b f = undefined
                  


sellPelts :: Baggage -> Fort -> [Int]
sellPelts b f  = map ceiling . 
  where
    fortSell
    m1 = (/) 100 $ floor ((0.2 * rnd + 0.7) * 100 + 0.5)
    e1 = (/) 100 $ floor ((0.2 * rnd + 0.65) * 100 + 0.5)
    b1 = (/) 100 $ floor ((0.2 * rnd + 0.75) * 100 + 0.5)
    d1 = (/) 100 $ floor ((0.2 * rnd + 0.8) * 100 + 0.5)
    m2 = (/) 100 $ floor ((0.3 * rnd + 0.85) * 100 + 0.5)
    e2 = (/) 100 $ floor ((0.15 * rnd + 0.80) * 100 + 0.5)
    b2 = (/) 100 $ floor ((0.2 * rnd + 0.9) * 100 + 0.5)
    m3 = (/) 100 $ floor ((0.15 * rnd + 1.05) * 100 + 0.5)
    d3 = (/) 100 $ floor ((0.25 * rnd + 1.10) * 100 + 0.5)
    rnd = getStdRandom(random) :: IO Double
                     




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

oneTwoThree :: IO Fort
oneTwoThree = do
  answer <- ask "ANSWER 1, 2 or 3."
  case (head answer) of
   '1' -> return Hochelaga
   '2' -> return Stadacona
   '3' -> return NewYork
   _ -> oneTwoThree

turn :: Baggage -> IO ()
turn = undefined
  


main :: IO ()
main = do
  intro
  putStrLn "DO YOU WISH TO TRADE FURS?"
  startGame <- yesNo
  if startGame
    then do
         turn (Baggage 0 0 0 0 600)
    else exitSuccess
