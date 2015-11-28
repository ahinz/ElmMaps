module Web where

import Maps.Core exposing (..)
import Maps.Geography exposing (..)
import Maps.Render exposing (..)
import Maps.Events exposing (mapUpdate, MapAction)
import Maps.Layer as L

import Html exposing (..)
import Effects exposing (Effects, Never)

import StartApp

import Effects exposing (Effects, Never)

import Html.Attributes exposing (style, class, id)
import Html.Events exposing (onClick)

osmTileLayer =
  L.createTileLayer
     { url = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
     , tileSize = {w=256, h=256}
     , minZoom = zoomLevel 8
     , maxZoom = zoomLevel 18
     , opaque = True }

aMap : Map
aMap = { emptyMap | state = { center={lat=51.5216, lng=-0.2527}
                            , size={w=500, h=500}
                            , zoom=zoomLevel 9 }
       , layers = [osmTileLayer]
       }


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
