import System.IO
import Data.Char
import qualified Data.Text as T
import Conversation

trim :: [Char] -> [Char]
trim [] = []
trim (x:xs) = if x == '\n' then xs else [x] ++ trim xs

getResponse :: [Pair] -> String -> String
getResponse [] _ = "No entiendo eso"
getResponse ((k, v):xs) input = if map toLower input `elem` k then v else getResponse xs input

main = putStr "> " >> hFlush stdout >> getLine >>= output . getResponse responses . trim
  where output s = putStrLn s >> if s == snd (responses !! 3) then pure () else main
