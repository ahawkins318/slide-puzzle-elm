import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Array exposing (fromList, toIndexedList)
import List exposing (..)
import Keyboard exposing (KeyCode)
import Random 

main =
   Html.program { init = init, view = view, update = update, subscriptions = subscriptions }

-- MODEL

type Direction = Up | Down | Left | Right 

randomDirections : Random.Generator (List (Maybe Direction))
randomDirections =  Random.list 100 <| Random.map (\n -> fromKeycode n) (Random.int 37 40)

fromKeycode : Int -> Maybe Direction
fromKeycode code = case code of
   37 -> Just Left
   38 -> Just Up
   39 -> Just Right
   40 -> Just Down
   _  -> Nothing

type alias Model = {
   size: Int,
   data: List String
}

init : (Model, Cmd Msg)
init = (Model 9 (initModelData 9), Random.generate NewRandom randomDirections)

initModelData : Int -> List String
initModelData size =
   (map toString [1..size-1]) ++ ["-"]

displayWidth : Model -> Int
displayWidth model = floor (sqrt (toFloat (model.size)))

isSolved : Model -> Bool
isSolved model =
  let nums = filter (\x -> x /= "-") (model.data) 
      solvedNums = map toString (take (model.size-1) [1..model.size-1])
  in (length nums > 1) && (nums == solvedNums)


-- UPDATE

subscriptions : Model -> Sub Msg
subscriptions model = Keyboard.presses KeyPressed

type Msg = NewSize String | KeyPressed KeyCode | NewRandom (List (Maybe Direction))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
   case msg of 
      NewSize size -> case String.toInt size of
         Err msg -> ( model, Cmd.none)
         Ok val -> ( { model | size = val, data = initModelData val }, Random.generate NewRandom randomDirections)
      KeyPressed code -> ( moveModel model (fromKeycode code), Cmd.none )
      NewRandom dirs -> 
        let shuffled = foldl (\d m -> moveModel m d) model dirs
        in (shuffled, Cmd.none)

moveModel : Model -> (Maybe Direction) -> Model
moveModel model direction = 
   case direction of 
      Nothing -> model
      Just d -> case (indicesToSwap model d) of
        Just (i,j) -> { model | data = swapIndices i j (model.data) }
        Nothing -> model

indicesToSwap : Model -> Direction -> Maybe (Int, Int)
indicesToSwap model direction = 
   let blankIndex = head << map fst << filter (\(i,x) -> x == "-") << toIndexedList <| fromList (model.data)
   in case blankIndex of 
      Nothing -> Nothing
      Just i -> 
         let j = adjacentIndex model i direction in
            if (isValidMove model i j) 
            then Just (i,j) 
            else Nothing

adjacentIndex : Model -> Int -> Direction -> Int
adjacentIndex model i d = 
   case d of
      Up    -> i + displayWidth model
      Down  -> i - displayWidth model
      Left  -> i + 1
      Right -> i - 1

isValidMove : Model -> Int -> Int -> Bool
isValidMove model i j = (isValidIndex model j) && not (indicesSpanLinebreak model i j)

isValidIndex : Model -> Int -> Bool
isValidIndex model i = (i >= 0) && (i < model.size)

indicesSpanLinebreak : Model -> Int -> Int -> Bool
indicesSpanLinebreak model i j = 
   ( abs (i - j) == 1) && 
   ( (Basics.max i j) % (displayWidth model) == 0 ) 

swapIndices : Int -> Int -> List a -> List a
swapIndices i j xs = 
   let (i',j') = ((Basics.min i j), (Basics.max i j))
      in let left   = (take i' xs)
             middle = (take (j'-i'-1) (drop (i'+1) xs))
             right  = (drop (j'+1) xs) 
         in case (elementAt i' xs, elementAt j' xs) of
            (Just ei, Just ej) -> left ++ [ej] ++ middle ++ [ei] ++ right
            _ -> xs

elementAt : Int -> List a -> Maybe a
elementAt i xs = head (drop i xs)

-- VIEW

view : Model -> Html Msg
view model = div [] [ header, inputs, puzzle model, alert model ]

header : Html Msg
header = 
    div [class "jumbotron"]  [
       h1 [class "text-center"] [ text "Sliding Puzzle!" ],
       h5 [class "text-center"] [ text "Use arrow keys to play the puzzle" ]
    ]

inputs : Html Msg
inputs = 
    div [class "container"] [ div [class "col-md-3"] [ div [class "panel panel-default"] [ div [class "panel-body"] [
       label [ for "size-input"] [ text "Size" ],
       input [ id "size-input", class "form-control", placeholder "enter new puzzle size", onInput NewSize ] []
    ]]]]

puzzle : Model -> Html Msg
puzzle model = 
   let w = displayWidth model
       c = "col-md-" ++ (toString w) in 
     div [class "container"] [ div [class c] [ div [class "panel panel-default"] [ div [class "panel-body"] [ div [class "container"] 
        (map viewRow (splitEvery w model.data))
     ]]]]

alert : Model -> Html Msg
alert model = 
   div [class "alert alert-success", hidden (not (isSolved model))] [ text "You WIN!!!!!!!!!" ]

-- VIEW HELPERS

viewRow : List String -> Html Msg
viewRow strs = div [class "row"]  (map viewCol strs)

viewCol : String -> Html Msg
viewCol str  = div [class "col-md-1"] [ text str ]

splitEvery : Int -> List a -> List (List a)
splitEvery n xs =
   case xs of
      [] -> []
      xs -> let (a,b) = (take n xs, drop n xs) in a :: splitEvery n b

-- sneak in the stylesheet while debugging - not needed after compiled
--stylesheet = let tag = "link" attrs = [ attribute "rel" "stylesheet", attribute "property" "stylesheet", attribute "href" "//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css" ] children = [] in node tag attrs children
