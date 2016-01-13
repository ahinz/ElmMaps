module Web where

import Maps.Core exposing (..)
import Maps.Geography exposing (..)
import Maps.Render exposing (..)
import Maps.Events exposing (mapUpdate)
import Maps.Layer as L
import Maps.Vector as V
import Maps.Marker as Mk

import Html exposing (..)
import Effects exposing (Effects, Never)

import Graphics.Collage as GC
import Color as C

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

fillColor = C.rgba 255 0 51 0.5
aCircle = V.createCircle {lat=51.508, lng=-0.11} 500 (GC.filled fillColor)

circleLayer = V.shapesToLayer [aCircle]


aMap : Map
aMap = { emptyMap | state = { center={lat=51.5216, lng=-0.2527}
                            , size={w=500, h=500}
                            , zoom=zoomLevel 9 }
       , layers = [circleLayer, osmTileLayer]
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
