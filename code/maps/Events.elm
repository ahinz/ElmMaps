module Maps.Events where

import Json.Decode as Json exposing ((:=))

import Effects exposing (Effects, Never)
import Html exposing (Attribute)
import Html.Events exposing (onWithOptions)

import Maps.Core exposing (..)
import Maps.Geometry exposing (..)
import Maps.Geography exposing (..)

type MapAction = DblClick Point
               | MouseDown Point
               | MouseUp Point
               | MouseMoved Point
               | MouseDragged Point Point

translatePixelPointToProjPt : Map -> Point -> Point
translatePixelPointToProjPt {tileSize, size, zoom, center} {x,y} =
  let
    pscale = scale zoom
    dw = size.w / 2.0 / tileSize.w
    dh = size.h / 2.0 / tileSize.h
    d = (point (x / tileSize.w - dw) (y / tileSize.w - dh))
    c = (tileAtPoint zoom (project center))

    c' = d `addPoint` c
  in
    pointAtTile zoom c'

translatePixelPointToLatLng : Map -> Point -> LatLng
translatePixelPointToLatLng m p =
  unproject (translatePixelPointToProjPt m p)

clickEventDecoder =
  Json.object2 (\x y -> {x=x,y=y})
      ("x" := Json.float)
      ("y" := Json.float)

decodeToClickOrDie : (Point -> MapAction) -> Json.Value -> MapAction
decodeToClickOrDie f v =
  case (Json.decodeValue clickEventDecoder v) of
    Ok p -> f p
    Err e -> Debug.crash e

opts = { stopPropagation = True
       , preventDefault = True }

decodable : String -> (Point -> MapAction) -> Signal.Address MapAction -> Attribute
decodable evt f addr =
  onWithOptions evt opts Json.value ((decodeToClickOrDie f) >> (Signal.message addr))

onDblClick : Signal.Address MapAction -> Attribute
onDblClick = decodable "dblclick" DblClick

onMouseDown : Signal.Address MapAction -> Attribute
onMouseDown = decodable "mousedown" MouseDown

onMouseMoved : Signal.Address MapAction -> Attribute
onMouseMoved = decodable "mousemove" MouseMoved

onMouseUp : Signal.Address MapAction -> Attribute
onMouseUp = decodable "mouseup" MouseUp

incZoom : ZoomLevel -> ZoomLevel
incZoom (ZoomLevel zl) = zoomLevel (min 22 (truncate (zl + 1)))

updateEventState : (EventState -> EventState) -> Map -> Map
updateEventState f m =
  { m | events=f m.events}

setDragAnchorPoint : Point -> Map -> Map
setDragAnchorPoint p =
  updateEventState (\x -> { x | lastPoint = Just p })

stopDragging : Map -> Map
stopDragging =
  updateEventState (\x -> { x | lastPoint = Nothing })

dragEvent : Point -> Map -> (Map, Maybe MapAction)
dragEvent curPt m =
  case m.events.lastPoint of
    Just prevPt ->
      let
        dragEvent = Debug.log "drag" (MouseDragged prevPt curPt)
        newMap = setDragAnchorPoint curPt m
      in
        if (distSq curPt prevPt) > 1.0 then
          (newMap, Just dragEvent)
        else
          (m, Nothing)
    Nothing -> (m, Nothing)

mapUpdate : MapAction -> Map -> (Map, Effects MapAction)
mapUpdate action map =
  let
    noeff = \x -> (x, Effects.none)
  in
    case action of
      MouseDragged p1 p2 ->
        let
          p1' = translatePixelPointToProjPt map p1
          p2' = translatePixelPointToProjPt map p2

          delta = p2' `subtractPoint` p1'
          c = project map.center

          c' = c `subtractPoint` delta
        in
          noeff { map | center = unproject c' }
      MouseDown pt -> noeff (setDragAnchorPoint pt map)
      MouseUp _ -> noeff (stopDragging map)
      MouseMoved pt ->
        case dragEvent pt map of
          (m', Just a) -> mapUpdate a m'
          (m', Nothing) -> noeff m'
      DblClick pt ->
        let
          ll = translatePixelPointToLatLng map pt
        in
          noeff {map | center=ll, zoom=incZoom map.zoom}
