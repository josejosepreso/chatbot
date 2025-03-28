module Conversation ( responses
                    , Pair
                    ) where

import Data.Time.Format
import Data.Time
import Data.Maybe
import System.IO.Unsafe

type Pair = ([String], String)

days = [ ("Sat", "Sabado")
       , ("Sun", "Domingo")
       , ("Mon", "Lunes")
       , ("Tue", "Martes")
       , ("Wed", "Miercoles")
       , ("Thu", "Jueves")
       , ("Fri", "Viernes")
       ]

getValue :: (Eq a) => [(a, a)] -> a -> Maybe a
getValue [] _ = Nothing
getValue ((x, y):xs) input = if x == input then Just y else getValue xs input

responses :: [Pair]
responses = [ (["hola", "buenas", "buen dia"], "Hola, como estas?")
             , (["que dia es hoy?"], "Hoy es " ++ dayName)
             , (["estoy bien"], "me alegro")
             , (["adios", "nos vemos", "hasta luego"], "hasta luego.")
             , (["gracias"], "de nada. Estoy aqui para ayudarte. Dime si necesitas ayuda en algo mas.")
             ]
  where dayName = unsafePerformIO $ getZonedTime >>= pure . fromJust . getValue days . formatTime defaultTimeLocale "%a"
