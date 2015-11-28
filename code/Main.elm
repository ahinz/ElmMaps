module Web where

import Maps exposing (..)
import Geography exposing (..)
import Render exposing (..)
import MapEvents exposing (mapUpdate, MapAction)

import Html exposing (..)
import Effects exposing (Effects, Never)

import StartApp

import Effects exposing (Effects, Never)

import Html.Attributes exposing (style, class, id)
import Html.Events exposing (onClick)

aMap = { emptyMap | center={lat=51.5216, lng=-0.2527}
       , size={w=500, h=500}
       , tileSize={w=256, h=256}
       , zoom=zoomLevel 9 }


view : Signal.Address MapAction -> Map -> Html
view address model =
  div [ class "container" ]
    [ div [ id "map"
          , mapStyle] [mapView address model]
    ]

mapStyle = style [("width", "500px")
                 ,("height", "500px")
                 ,("background", "red")]


app  = StartApp.start
    { update = mapUpdate
    , view = view
    , init = (aMap, Effects.none)
    , inputs = []
    }

main : Signal Html.Html
main =
  app.html
