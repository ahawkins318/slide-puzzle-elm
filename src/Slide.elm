module Slide exposing (..)

import Html.App
import Model  exposing (init)
import Update exposing (update, subscriptions)
import View   exposing (view)

main = Html.App.program { init = init, view = view, update = update, subscriptions = subscriptions }