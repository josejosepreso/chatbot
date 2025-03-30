module Bot ( responses
           , responsesPattern
           ) where

import Data.Time.Format
import Data.Time
import Data.Maybe
import System.IO.Unsafe
import Data.Char
import Data.Maybe
import Text.Regex.Posix

days = [ ("Sat", "Sabado")
       , ("Sun", "Domingo")
       , ("Mon", "Lunes")
       , ("Tue", "Martes")
       , ("Wed", "Miercoles")
       , ("Thu", "Jueves")
       , ("Fri", "Viernes")
       ]

getValue :: Eq a => [(a, a)] -> a -> Maybe a
getValue [] _ = Nothing
getValue ((x, y):xs) input
  | x == input = Just y
  | otherwise = getValue xs input

matchInt :: String -> String -> Maybe (String, String)
matchInt [] [] = Nothing
matchInt [] result = Just (result, "")
matchInt (x:xs) result
  | isDigit x = matchInt xs (result ++ [x])
  | and [null result, not . isDigit $ x] = Nothing
  | otherwise = Just (result, (x:xs))

group :: String -> [String]
group [] = []
group (x:xs)
  | not . isDigit $ x = [[x]] ++ group xs
  | otherwise = let pair = fromJust . matchInt (x:xs) $ []
                    digit = fst pair
                    rest = snd pair
                in [digit] ++ group rest

calculator :: [String] -> Int
calculator [a] = read a :: Int
calculator (x:y:z:xs) = calculator ([show $ result x y z] ++ xs)
  where result a b c
          | b == "+" = n + m
          | b == "-" = n - m
          | b == "/" = n `div` m
          | otherwise = n * m
          where n = read a :: Int
                m = read c :: Int

getOperationResult :: String -> String
getOperationResult input = "El resultado de la operacion es: "
                           ++ ( show
                                . calculator
                                . group
                                . getThird
                                $ getOperation
                              )
  where getOperation = input =~ "cuanto es *" :: (String, String, String)
        getThird (a, b, c) = c

responses = [ (["hola", "buenas", "buen dia"], "Hola, como estas?")
             , (["que dia es hoy?"], "Hoy es " ++ dayName)
             , (["estoy bien"], "me alegro")
             , (["adios", "nos vemos", "hasta luego"], "hasta luego.")
             , (["gracias"], "de nada. Estoy aqui para ayudarte. Dime si necesitas ayuda en algo mas.")
             ]
  where dayName = unsafePerformIO
                  $ getZonedTime
                  >>= pure
                  . fromJust
                  . getValue days
                  . formatTime defaultTimeLocale "%a"

responsesPattern :: [(String, (String -> String))]
responsesPattern = [ ("cuanto es *", getOperationResult)
                   ]
