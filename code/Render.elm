module Render where

import Maps exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, class, id, src)


aMap = { center={lat=51.5216, lng=-0.2527}
       , size={w=500, h=500}
       , tileSize={w=256, h=256}
       , zoom=9 }

origin = pixelOrigin aMap
pos = tilePosition aMap.tileSize origin

imgTag : TileCoord -> Html
imgTag {x,y,z,xoff,yoff} =
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

tiles = tilesForView 9 (project aMap.center) aMap.size

mapView : Map -> Html
mapView {center, size, zoom} =
  div [ class "map-container"
      , style [ ("position", "relative")
              , ("overflow", "hidden")
              , ("width", "100%")
              , ("height", "100%")]]
      [ div [] (List.map imgTag tiles) ]
