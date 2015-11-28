module MapEvents where

import Json.Decode as Json exposing ((:=))

import Effects exposing (Effects, Never)
import Html exposing (Attribute)
import Html.Events exposing (on)

import Maps exposing (..)

type MapAction = DblClick Point

translatePixelPointToLatLng : Map -> Point -> LatLng
translatePixelPointToLatLng {tileSize, size, zoom, center} {x,y} =
  let
    pscale = scale zoom
    dw = size.w / 2.0 / tileSize.w
    dh = size.h / 2.0 / tileSize.h
    d = (point (x / tileSize.w - dw) (y / tileSize.w - dh))
    c = (tileAtPoint zoom (project center))

    c' = d `addPoint` c
  in
    unproject (pointAtTile zoom c')

clickEventDecoder =
  Json.object2 (\x y -> {x=x,y=y})
      ("x" := Json.float)
      ("y" := Json.float)

decodeToClickOrDie : Json.Value -> MapAction
decodeToClickOrDie v =
  case (Json.decodeValue clickEventDecoder v) of
    Ok p -> DblClick p
    Err e -> Debug.crash e

onDblClick : Signal.Address MapAction -> Attribute
onDblClick address =
  on "dblclick" Json.value (decodeToClickOrDie >> (Signal.message address))


incZoom : ZoomLevel -> ZoomLevel
incZoom = ((+) 1) >> (min 22)

mapUpdate : MapAction -> Map -> (Map, Effects MapAction)
mapUpdate action map =
  case action of
    DblClick pt -> let ll = translatePixelPointToLatLng map pt
                     in
                       ({map | center=ll, zoom=incZoom map.zoom}, Effects.none)
