module Direction exposing (..)
import Random 

type Direction = Up | Down | Left | Right 

fromKeycode : Int -> Maybe Direction
fromKeycode code = case code of
   37 -> Just Left
   38 -> Just Up
   39 -> Just Right
   40 -> Just Down
   _  -> Nothing

arrowCodeGenerator : Int -> Random.Generator (List Int)
arrowCodeGenerator size = Random.list size (Random.int 37 40)