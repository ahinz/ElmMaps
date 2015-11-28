module Web where

import Maps exposing (..)
import Render exposing (..)

import Html exposing (..)
import Effects exposing (Effects, Never)

import StartApp

import Effects exposing (Effects, Never)

import Html.Attributes exposing (style, class, id)
import Html.Events exposing (onClick)


type alias Model = { frame: Int }

emptyModel : Model
emptyModel = { frame = 0 }

type Action = Increment | Decrement

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Increment -> ({ model | frame = model.frame + 1 }, Effects.none)
    Decrement -> ({ model | frame = model.frame - 1 }, Effects.none)

view : Signal.Address Action -> Model -> Html
view address model =
  div [ class "container" ]
    [ div [ id "map"
          , mapStyle] [mapView aMap]
    ]

mapStyle = style [("width", "500px")
                 ,("height", "500px")
                 ,("background", "red")]


app  = StartApp.start
    { update = update
    , view = view
    , init = (emptyModel, Effects.none)
    , inputs = []
    }

main : Signal Html.Html
main =
  app.html
