module Model exposing (..)

import Random
import Direction
import Messages exposing (..)
import List exposing (map, filter, take, length)


type alias Model = {
   size: Int,
   data: List String,
   blankIndex: Int     --turns out it's a lot easier to explicitly track the location of the blank space vs searching for it in the list
}

init : (Model, Cmd Msg)
init = (unshuffledModel 9, shuffleCmd 9)

solvedModelData : Int -> List String
solvedModelData size = (map toString [1..size-1]) ++ ["-"]

unshuffledModel : Int -> Model
unshuffledModel size = Model size (solvedModelData size) (size-1)

shuffleCmd : Int -> Cmd Msg
shuffleCmd size = Random.generate constructRandomDirsMsg << Direction.arrowCodeGenerator <| Basics.min 2000 (20*size)


displayWidth : Model -> Int
displayWidth model = floor (sqrt (toFloat (model.size)))

isSolved : Model -> Bool
isSolved model = (length model.data > 2) && (model.data == (solvedModelData model.size))