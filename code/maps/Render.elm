module Maps.Render where

import Html exposing (..)
import Html.Attributes exposing (style, class, id, src)

import Maps.Core exposing (..)
import Maps.Events exposing (..)
import Maps.Geometry exposing (..)
import Maps.Geography exposing (..)

import Maps.Layer exposing (..)

mapView : Signal.Address MapAction -> Map -> Html
mapView addr map =
  let
    st = map.state
    layers = List.reverse map.layers |>
             List.map (\layer -> layer.renderer st addr)
  in
    div [ class "map-container"
        , style [ ("position", "relative")
                , ("overflow", "hidden")
                , ("width", "100%")
                , ("height", "100%")]
        , onDblClick addr
        , onMouseMoved addr
        , onMouseUp addr
        , onMouseDown addr]
    <| layers
