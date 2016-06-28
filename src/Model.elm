module Model exposing (..)

import Random
import Direction
import Messages exposing (..)
import List exposing (map, filter, take, length)


type alias Model = {
   size: Int,
   data: List String,
   blankIndex: Int
}

init : (Model, Cmd Msg)
init = requestShuffledModel 9

solvedModelData : Int -> List String
solvedModelData size = (map toString [1..size-1]) ++ ["-"]

requestShuffledModel : Int -> (Model, Cmd Msg)
requestShuffledModel size = 
   let command = Random.generate constructRandomDirsMsg << Direction.arrowCodeGenerator <| Basics.min 2000 (20*size)
   in ( Model size (solvedModelData size)  (size-1) , command )


displayWidth : Model -> Int
displayWidth model = floor (sqrt (toFloat (model.size)))

isSolved : Model -> Bool
isSolved model = (length model.data > 2) && (model.data == (solvedModelData model.size))