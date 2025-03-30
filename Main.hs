import System.IO
import Data.Char
import Bot
import Text.Regex.Posix

trim :: [Char] -> [Char]
trim [] = []
trim (x:xs)
  | x == '\n' = xs
  | otherwise = [x] ++ trim xs

getResponseByPattern [] _ = "No entiendo eso"
getResponseByPattern ((k, fn):xs) input
  | map toLower input =~ k = fn input
  | otherwise = getResponseByPattern xs input

getResponse [] input = getResponseByPattern responsesPattern input
getResponse ((k, v):xs) input
  | map toLower input `elem` k = v
  | otherwise = getResponse xs input
    
main = putStr "> "
       >> hFlush stdout
       >> getLine
       >>= output
       . getResponse responses
       . trim
  where output s = putStrLn s
                   >> if s == snd (responses !! 3)
                      then pure ()
                      else main
