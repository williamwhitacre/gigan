{-

EXAMPLE 0: How to use the basic features of Core in one example. Makes a counter with an
automatic mode that uses the OrbiterTask utilities, and sets up a basic Orbiter. The architectural
weight needed to initially set up an Orbiter is a bit heavier than that which you'd see in a
StartApp application. The added flexibility of the Orbiter is visible here, but not very heavily
excercised.

-}

import Gigan.Core exposing (..)
import Html exposing (Html, div, span, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

import Window

import Signal
import Task exposing (Task)

import Time exposing (Time)


type Action =
  Change Int
  | AutoOn Int
  | AutoDo
  | AutoOff
  | CurrentWidth Int


type alias Model =
  { stop : Bool
  , auto : Int
  , value : Int
  , width : Int
  }


autoIncrement : Model -> TaskDispatchment () Action
autoIncrement model =
  if not model.stop && model.auto /= 0 then
    dispatchTasks [orbiterBlindAgent (orbiterAgentSuccess [AutoDo]) (Task.sleep 1000)]
  else
    dispatchTasks []


styleOut : Html.Attribute
styleOut =
  style
    [ ("position", "absolute")
    , ("top", "20px")
    , ("left", "20px")
    ]


model0 : Model
model0 =
  { stop = False
  , auto = 0
  , value = 0
  , width = 0
  }


autoActions : Model -> Int -> List Action
autoActions model amount =
  [if model.auto == 0 then AutoOn amount else AutoOff]


stopOr : Model -> String -> String
stopOr model say =
  if model.auto /= 0 && not model.stop then "stop" else say


present : Signal.Address (List Action) -> Time -> Model -> ViewOutput Action Html ()
present address now model =
  div
    [ styleOut ]
    [ div
        [ ]
        [ button [ onClick address (autoActions model -1) ] [ text (stopOr model "--") ]
        , button [ onClick address [Change -1] ]            [ text "-" ]
        , button [ onClick address [Change 1] ]             [ text "+" ]
        , button [ onClick address (autoActions model 1) ]  [ text (stopOr model "++") ]

        , span [] [ text (toString model.value) ]
        ]
    , div
        [ ]
        [ Html.hr [] []
        , text ((++) "window width last changed to " <| toString model.width)
        , Html.br [] []
        , text ((++) "the timestamp of the last executed action was " <| toString now)
        ]
    ]

  |> presented


update : Action -> Time -> Model -> UpdatedModel Action Model ()
update action now model =
  case action of
    CurrentWidth width ->
      { model | width = width }
      |> updated

    Change amount ->
      { model | value = model.value + amount }
      |> updated

    AutoOn amount ->
      { model | stop = False, auto = amount }
      |> \model' -> updated model'
      |> withDispatchment (autoIncrement model')

    AutoDo ->
      update (Change model.auto) now model
      |> withDispatchment (autoIncrement model)

    AutoOff ->
      { model | stop = True, auto = 0 }
      |> updated



output : OrbiterOutput Action Model Html ()
output =
  (defProgram present update model0)
    `withSequenceInputs`
      [ Signal.map (CurrentWidth >> flip (::) []) Window.width ]
    `orbitsWithWork`
      nilTask
    +--> itself


main : Signal Html.Html
main = output.view'


port sink : Signal (Task z ())
port sink = sieve output
