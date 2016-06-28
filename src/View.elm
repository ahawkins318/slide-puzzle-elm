module View exposing (view)

import Model exposing (..)
import Messages exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List exposing (map, take, drop)


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
       input [ id "size-input", class "form-control", placeholder "enter new puzzle size", onInput constructSizeMsg ] []
    ]]]]


puzzle : Model -> Html Msg
puzzle model = 
   let width = displayWidth model in
   div [class "container"] [ div [class ("col-md-" ++ (toString width))] [ div [class "panel panel-default"] [ div [class "panel-body"] [ div [class "container"] 
      (map viewRow (splitEvery width model.data))
   ]]]]


alert : Model -> Html Msg
alert model = div [class "alert alert-success", hidden (not (isSolved model))] [ text "You WIN!!!!!!!!!" ]


viewRow : List String -> Html Msg
viewRow strs = div [class "row"]  (map viewCol strs)

viewCol : String -> Html Msg
viewCol str  = div [class "col-md-1"] [ text str ]


splitEvery : Int -> List a -> List (List a)
splitEvery n xs = case xs of
   [] -> []
   xs -> let (a,b) = (take n xs, drop n xs) in a :: splitEvery n b

--debug only
--stylesheet = let tag = "link" attrs = [ attribute "rel" "stylesheet", attribute "property" "stylesheet", attribute "href" "//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css" ] children = [] in node tag attrs children
