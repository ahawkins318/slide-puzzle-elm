module Update exposing (update, subscriptions)

import Model exposing (..)
import Messages exposing (..)
import Direction exposing (..)

import List exposing (foldl, head, drop)
import Array exposing (fromList, toList, set, get)
import Keyboard


subscriptions : Model -> Sub Msg
subscriptions model = Keyboard.presses constructKeyMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of 
   Noop            -> ( model, Cmd.none )
   NewSize size    -> requestShuffledModel size
   KeyPressed dir  -> ( moveModel model dir, Cmd.none )
   RandomDirs dirs -> ( shuffleModel model dirs, Cmd.none )


moveModel : Model -> Direction -> Model
moveModel model direction = 
   case (indicesToSwap model direction) of
     Just (i,j) -> swapModelData model j
     Nothing    -> model


shuffleModel : Model -> (List Direction) -> Model
shuffleModel model dirs = foldl (\d m -> moveModel m d) model dirs


indicesToSwap : Model -> Direction -> Maybe (Int, Int)
indicesToSwap model direction = 
   let (i,j) = (model.blankIndex, adjacentIndex model direction) in
      if (indicesSpanLinebreak model i j)
      then Nothing 
      else Just (i,j) 


adjacentIndex : Model -> Direction -> Int
adjacentIndex model direction = case direction of
   Up    -> model.blankIndex + displayWidth model
   Down  -> model.blankIndex - displayWidth model
   Left  -> model.blankIndex + 1
   Right -> model.blankIndex - 1


indicesSpanLinebreak : Model -> Int -> Int -> Bool
indicesSpanLinebreak model i j = ( abs (i - j) == 1) && ( (Basics.max i j) % (displayWidth model) == 0 ) 


swapModelData : Model -> Int -> Model
swapModelData model adjacentIndex = 
   let data = fromList (model.data) in
   case (get adjacentIndex data) of 
      Just adjacentElement -> 
         let swappedData = toList << set adjacentIndex "-" << set model.blankIndex adjacentElement <| data
         in Model model.size swappedData adjacentIndex
      Nothing -> model