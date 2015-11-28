module Maps.Render where

import Html exposing (..)
import Html.Attributes exposing (style, class, id, src)

import Maps.Core exposing (..)
import Maps.Events exposing (..)
import Maps.Geometry exposing (..)
import Maps.Geography exposing (..)



imgTag : (Point -> Point) -> TileCoord -> Html
imgTag pos {x,y,z,xoff,yoff} =
  let
    offset = pos (point (toFloat xoff) (toFloat yoff))

    isrc = "http://c.tile.openstreetmap.org/" ++ (toString z) ++ "/" ++ (toString x) ++ "/" ++ (toString y) ++ ".png"
    istyle = style [ ("width", "256px")
                   , ("height", "256px")
                   , ("position", "absolute")
                   , ("left", (toString offset.x) ++ "px")
                   , ("top", (toString offset.y) ++ "px")
                   ]
  in
    img [ src isrc
        , istyle ] []

dontSelectMeStyles = [ ("-webkit-user-select", "none")
                     , ("-moz-user-select", "none")
                     , ("user-select", "none")
                     , ("-webkit-user-drag", "none")]

mapView : Signal.Address MapAction -> Map -> Html
mapView addr map =
  let
    tiles = tilesForView map.zoom (project map.center) map.size

    origin = pixelOrigin map
    pos = tilePosition map.tileSize origin

    itag = imgTag pos
  in
    div [ class "map-container"
        , style ([ ("position", "relative")
                 , ("overflow", "hidden")
                 , ("width", "100%")
                 , ("height", "100%")] ++ dontSelectMeStyles)
        , onDblClick addr
        , onMouseMoved addr
        , onMouseUp addr
        , onMouseDown addr]
    [ div [] (List.map itag tiles) ]
