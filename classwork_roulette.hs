import System.Random

numbers = [0..36]
red = [1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36]
black = [2,4,6,8,10,11,13,15,17,20,22,24,26,28,29,31,33,35]
green = [0]

determineColor :: Int -> String
determineColor num
  | num `elem` red = "red"
  | num `elem` black = "black"
  | num `elem` green = "green"
  | otherwise = "invalid"

evenOrOdd :: Int -> String
evenOrOdd num
  | even num = "even"
  | odd num = "odd"
  | otherwise = "invalid"

determineHalf :: Int -> String
determineHalf num
  | num `elem` [1..18] = "1st half"
  | num `elem` [19..36] = "2nd half"
  | otherwise = "invalid"

determineDozen :: Int -> String
determineDozen num
  | num `elem` [1..12] = "1st dozen"
  | num `elem` [13..24] = "2nd dozen"
  | num `elem` [25..36] = "3rd dozen"
  | otherwise = "invalid"

determineColumn :: Int -> String
determineColumn num
  | num `elem` [1,4,7,10,13,16,19,22,25,28,31,34] = "1st column"
  | num `elem` [2,5,8,11,14,17,20,23,26,29,32,35] = "2nd column"
  | num `elem` [3,6,9,12,15,18,21,24,27,30,33,36] = "3rd column"
  | otherwise = "invalid"

randomNumber :: IO Int
randomNumber = randomRIO (0, 36)

compareResults :: Int -> String -> String -> IO ()
compareResults num choice bet
  | choice == "green" && determineColor num == "green" = putStrLn $ "You won " ++ show (read bet * 36)
  | choice == "red" && determineColor num == "red" = putStrLn $ "You won " ++ show (read bet * 2)
  | choice == "black" && determineColor num == "black" = putStrLn $ "You won " ++ show (read bet * 2)
  | evenOrOdd num == choice = putStrLn $ "You won " ++ show (read bet * 2)
  | determineHalf num == choice = putStrLn $ "You won " ++ show (read bet * 2)
  | determineDozen num == choice = putStrLn $ "You won " ++ show (read bet * 3)
  | determineColumn num == choice = putStrLn $ "You won " ++ show (read bet * 3)
  | choice == show num = putStrLn $ "You won " ++ show (read bet * 36)
  | otherwise = putStrLn "You lost!"

getChoice :: IO String
getChoice = do
  choice <- getLine
  if choice `elem` ["green", "red", "black", "even", "odd", "1st half", "2nd half", "1st dozen", "2nd dozen", "3rd dozen", "1st column", "2nd column", "3rd column"] || choice `elem` map show numbers
    then return choice
    else do
      putStrLn "Invalid choice! Please enter a valid choice: "
      getChoice

getBet :: IO String
getBet = do
  bet <- getLine
  if bet `elem` map show [1..1000]
    then return bet
    else do
      putStrLn "Invalid bet! Minimum bet is 1, maximum is 1000. Please enter a valid bet: "
      getBet

-- Possible player inputs
-- 1. (0..36)
-- 2. (green, red, black)
-- 3. (even, odd)
-- 4. (1st half, 2nd half)
-- 5. (1st dozen, 2nd dozen, 3rd dozen)
-- 6. (1st column, 2nd column, 3rd column)
-- Bet (1..1000)
main :: IO ()
main = do
  putStrLn "Bets are opened!"
  putStrLn "Enter your bet: "
  bet <- getBet
  putStrLn "Enter your choice: "
  choice <- getChoice
  num <- randomNumber
  putStrLn "Bets are closed!"
  putStrLn $ "Winning number is: " ++ show num
  putStrLn $ determineColor num ++ "; " ++ evenOrOdd num ++ "; " ++ determineHalf num ++ "; " ++ determineDozen num ++ "; " ++ determineColumn num 
  compareResults num choice bet
  putStrLn "Thanks for playing!"
