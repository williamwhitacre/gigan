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

import Signal
import Task exposing (Task)

import Time exposing (Time)


type Action =
  Increment
  | Decrement
  | Stop
  | Auto Int


type alias Model =
  { stop : Bool
  , auto : Int
  , value : Int
  }


autoIncrement : Int -> OrbiterTask () Action
autoIncrement amount =
  orbiterBlindAgent (orbiterAgentSuccess [Auto amount]) (Task.sleep 1000)


model0 : Model
model0 =
  { stop = False
  , auto = 0
  , value = 0
  }


present : Signal.Address (List Action) -> Time -> Model -> ViewOutput Action Html ()
present address now model =
  div []
    [ button
      [ onClick address [if model.auto /= 0 then Stop else Auto -1] ]
      [ text (if model.auto /= 0 then "stop" else "--") ]

    , button [ onClick address [Decrement] ] [ text "-" ]

    , span [] [ text (toString model.value) ]

    , button [ onClick address [Increment] ] [ text "+" ]

    , button
      [ onClick address [if model.auto /= 0 then Stop else Auto 1] ]
      [ text (if model.auto /= 0 then "stop" else "++") ]
    ]

  |> presented


update : Action -> Time -> Model -> UpdatedModel Action Model ()
update action now model =
  let
    model' =
      case action of
        Increment -> { model | value = model.value + 1 }
        Decrement -> { model | value = model.value - 1 }

        Stop ->
          if not model.stop then { model | stop = True, auto = 0 } else model

        Auto n ->
          if model.stop then { model | stop = False, auto = 0 }
          else { model | auto = n, value = model.value + n }

  in
    updated model'
    |> withTasks (if model'.auto /= 0 then [autoIncrement model'.auto] else [ ])


output : OrbiterOutput Action Model Html ()
output =
  (defProgram present update model0)
    `orbitsWithWork` nilTask
    +--> itself


main : Signal Html.Html
main = output.view'


port sink : Signal (Task z ())
port sink = sieve output
