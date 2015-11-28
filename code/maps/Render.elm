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
    layers = List.foldl (\layer (divs, stop) ->
                           if stop then
                             (divs, True)
                           else
                             ((layer.renderer st) :: divs, layer.opaque)) ([], False) map.layers
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
    <| fst layers
